package Chainsaw.dag

import Chainsaw._
import org.jgrapht._
import org.jgrapht.alg.connectivity._
import org.jgrapht.graph._
import spinal.core._

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

// TODO: appropriate metadata for consistency
case class IoGenerator(numericType: NumericType, direction: Direction)
  extends Combinational {
  override def name = if (direction == In) "in" else "out"

  override def impl(dataIn: Seq[Any]) = dataIn

  override def inputTypes = Seq(numericType)

  override def outputTypes = Seq(numericType)

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl

  override def comb(dataIn: Seq[Bits]) = dataIn
}

object InputVertex {
  def apply(numericType: NumericType)(implicit ref: Dag) = {
    val vertex = DagVertex(IoGenerator(numericType, In))
    vertex.setName(s"i_${ref.inputs.length}")
    ref.addVertex(vertex)
    ref.inputs += vertex
    vertex.out(0)
  }
}

object OutputVertex {
  def apply(numericType: NumericType)(implicit ref: Dag) = {
    val vertex = DagVertex(IoGenerator(numericType, Out))
    vertex.setName(s"o_${ref.outputs.length}")
    ref.addVertex(vertex)
    ref.outputs += vertex
    vertex.in(0)
  }
}

case class ConstantGenerator(numericType: NumericType, constant: Any)
  extends Combinational {

  val constantBits = numericType.toBigInt(constant)

  override def name = s"constant_${constant.hashCode()}".replace('-', 'N')

  override def comb(dataIn: Seq[Bits]) = Seq(B(constantBits, numericType.bitWidth bits))

  override def impl(dataIn: Seq[Any]) = Seq(constant)

  override def inputTypes = Seq(numericType)

  override def outputTypes = Seq(numericType)

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl
}

object ConstantVertex {
  def apply(numericType: NumericType, constant: Any)(implicit ref: Dag) = {
    val vertex = ConstantGenerator(numericType, constant).asVertex
    ref.addVertex(vertex)
    vertex.out(0)
  }
}

abstract class Dag()
  extends DirectedWeightedMultigraph[DagVertex, DagEdge](classOf[DagEdge]) with ChainsawGenerator {

  implicit val ref: Dag = this

  type V = DagVertex
  type E = DagEdge
  type Port = DagPort

  val inputs = ArrayBuffer[V]()
  val outputs = ArrayBuffer[V]()

  /** --------
   * construction methods
   * -------- */
  // override & deprecate the superclass method to warn users not to use it
  @deprecated
  override def addEdge(sourceVertex: V, targetVertex: V) = super.addEdge(sourceVertex, targetVertex)

  @deprecated
  override def addEdge(sourceVertex: V, targetVertex: V, e: E): Boolean = super.addEdge(sourceVertex, targetVertex, e)

  // provide our method
  def addEdge(source: Port, target: Port, weight: Double = 0): E = {
    require(source.direction == Out && target.direction == In)
    val e = new E(target.order, source.order)
    assert(super.addEdge(source.vertex, target.vertex, e))
    setEdgeWeight(e, weight)
    e
  }

  def addVertexWithDrivers(target: V, srcs: Port*): Unit = {
    require(srcs.forall(_.direction == Out))
    addVertex(target)
    srcs.zipWithIndex.foreach { case (port, i) => addEdge(port, target.in(i)) }
  }

  /** add a subgraph into this graph
   *
   * @param starts inputs of the subgraph will be connected to the starts
   * @return outputs of the subgraph which can be used for further construction
   */
  def addGraphBetween(subGraph: Dag, starts: Seq[Port], ends: Seq[Port]): Unit = {
    // check
    require(subGraph.inputs.length == starts.length, s"${subGraph.inputs.length} != ${starts.length}")
    require(starts.forall(_.direction == Out))
    require(subGraph.outputs.length == ends.length, s"${subGraph.outputs.length} != ${ends.length}")
    require(ends.forall(_.direction == In))
    // add
    val verticesMap = subGraph.vertexSet().asScala
      .filterNot(_.isIo(subGraph))
      .toSeq.map(v => v -> v.cloneTo(this)).toMap
    subGraph.edgeSet().asScala
      .filterNot(e => e.source(subGraph).isIo(subGraph) || e.target(subGraph).isIo(subGraph))
      .foreach { e =>
        val oldSourcePort = e.sourcePort(subGraph)
        val oldTargetPort = e.targetPort(subGraph)
        val sourcePort = verticesMap(oldSourcePort.vertex).out(oldSourcePort.order)
        val targetPort = verticesMap(oldTargetPort.vertex).in(oldTargetPort.order)
        addEdge(sourcePort, targetPort)
      }
    // link
    val startTargets = subGraph.inputs.map { in =>
      val target = verticesMap(in.targets(subGraph).head)
      val order = in.targetPorts(subGraph).head.order
      DagPort(target, order, In)
    }
    val endSources = subGraph.outputs.map { out =>
      val source = verticesMap(out.sources(subGraph).head)
      val order = out.sourcePorts(subGraph).head.order
      DagPort(source, order, Out)
    }
    starts.zip(startTargets).foreach { case (port, in) => addEdge(port, in) }
    ends.zip(endSources).foreach { case (port, out) => addEdge(out, port) }
  }

  /** --------
   * methods for retiming
   * -------- */
  def isComb: Boolean = edgeSet().asScala.forall(_.weight == 0)

  def makeComb(): Unit = if (!isComb) edgeSet().asScala.foreach(e => setEdgeWeight(e, 0.0))

  /** do retiming according to the retiming solution
   *
   * @see ''Parhi, Keshab K.. “Vlsi Digital Signal Processing Systems: Design And Implementation.” (2007).'' Chapter 4
   */
  def retiming(solution: Map[V, Int]): Unit = edgeSet().asScala.foreach { e =>
    val (targetValue, sourceValue) = (solution(getEdgeTarget(e)), solution(getEdgeSource(e)))
    val weight = targetValue + e.targetPort.relativeTime - (sourceValue + e.sourcePort.relativeTime) + e.weight
    setEdgeWeight(e, weight)
  }

  def autoPipeline(): Dag = {
    logger.info(s"cplex exist: ${cplexJarPath.exists()}")
    if (cplexJarPath.exists()) AutoPipelineCplex(this)
    else AutoPipeline(this)
  }

  /** --------
   * methods for rewriting
   * -------- */
  def flatten(): Dag = { // flatten all vertices which are Dags themselves
    Flatten(this)
    logger.info(s"\n----do retiming after flatten----")
    updateLatency()
    this
  }

  /** --------
   * methods for implementation
   * -------- */
  override def implH: ChainsawModule = {
    exportPng(name)
    DagImplH(this)
  }

  def setVerticesAsNaive(): Unit = vertexSet().asScala.foreach(_.gen.setAsNaive())

  /** --------
   * methods for getting metadata
   * -------- */
  override def inputTypes = inputs.map(_.gen.inputTypes.head)

  override def outputTypes = outputs.map(_.gen.outputTypes.head)

  // TODO: methods for generating frame format according to vertices and Dag topology
  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl

  var retimingInfo = Map[V, Int]()

  override def latency = { // TODO: better strategy
    val ret = if (implMode == Infinite) {
      val path = new alg.shortestpath.BFSShortestPath(this).getPath(inputs.head, outputs.head)
      path.getVertexList.asScala.prevAndNext { case (prev, next) => getEdge(prev, next).weight.toInt + next.gen.latency }.sum
    }
    else retimingInfo(outputs.head) - retimingInfo(inputs.head)

    logger.warn(s"Dag latency = $ret")
    ret
  }

  def updateLatency(): Unit = autoPipeline()

  def graphDone(): Unit = {
    updateLatency()
    doDrc()
  }

  def assureAcyclic(): Unit = assert(!new alg.cycle.CycleDetector(this).detectCycles(), "dag must be acyclic")

  def assureConnected(): Unit = {
    val ins = new ConnectivityInspector(this)
    assert(ins.isConnected,
      s"dag must be a connected graph, connected sets = \n ${ins.connectedSets().asScala.mkString("\n")}")
  }

  override def doDrc(): Unit = {
    logger.info(s"\n----drc started...----")
    super.doDrc()
    assureAcyclic()
    logger.info("acyclic assured!")
    assureConnected()
    logger.info("connected assured!")
    // all inPorts must be driven
    // skip as this will be found by implH anyway
    //    vertexSet().toSeq.diff(inputs).foreach { v =>
    //      v.inPorts.foreach(port => assert(edgeSet().exists(e => e.targetPort == port), s"inPort ${v.inPorts.indexOf(port)} of ${port.vertex.vertexName} has no driver"))
    //    }
    //    logger.info("all inports are driven!")
    vertexSet().asScala.toSeq.diff(outputs).foreach { v =>
      v.outPorts.foreach(port => if (!edgeSet().asScala.exists(e => e.sourcePort == port)) logger.warn(s"outPort ${v.outPorts.indexOf(port)} of  ${port.vertex.vertexName} is not used"))
    }
    val notTimed = retimingInfo.keys.toSeq.diff(vertexSet().asScala.toSeq)
    assert(notTimed.isEmpty, s"vertices ${notTimed.mkString(" ")} have no retiming value")
    logger.info("all vertices retimed!")
    logger.info(s"\n----drc done!----")
  }

  /** --------
   * methods for readability & visualization
   * -------- */
  def pathToString(path: GraphPath[V, E]) = {
    path.getVertexList.asScala.zip(path.getEdgeList.asScala)
      .map { case (v, e) => s"$v -> ${e.weight} -> " }.mkString("\n") +
      path.getEndVertex.toString
  }

  def exportPng(graphName: String = name): Unit = ToPng(this, graphName)

  override def toString = s"\nvertices:\n${vertexSet().asScala.mkString("\n")}" + s"edges:\n${edgeSet().asScala.map(_.toStringInGraph).mkString("\n")}"
}
