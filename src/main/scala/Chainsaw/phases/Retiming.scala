package Chainsaw.phases

import Chainsaw._
import spinal.core.GlobalData
import spinal.core.internals.{Phase, PhaseContext}

class Retiming() extends Phase {
  override def impl(pc: PhaseContext): Unit = {
    // TODO: more verifications by different generators
    logger.info("----start retiming----")
    require(GlobalData.get.toplevel.isInstanceOf[ChainsawBaseModule], "for retiming, the top level module must be a ChainsawBaseModule")
    val top = GlobalData.get.toplevel.asInstanceOf[ChainsawBaseModule]
    RetimingGraph(top).doRetiming(top)
    logger.info("-----end retiming-----")
  }

  override def hasNetlistImpact = true
}

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import spinal.core._
import spinal.core.internals._

import scala.collection.JavaConverters._
import scala.collection.mutable

/** designed for retiming
 *
 */
class RetimingGraph extends ExpressionGraph {

  /** from expression graph to component graph, for retiming and visualization
   */
  def toComponentGraph = { //
    val graph = new RetimingGraph
    edgeSet().asScala.foreach(e => {
      val src = getEdgeSource(e).asInstanceOf[BaseType]
      val srcVertex = if (inputs.contains(src)) {
        graph.addInput(src)
        src
      } else {
        graph.addVertex(src.component)
        src.component
      }
      val des = getEdgeTarget(e).asInstanceOf[BaseType]
      val desVertex = if (outputs.contains(des)) {
        graph.addOutput(des)
        des
      } else {
        graph.addVertex(des.component)
        des.component
      }
      val weight = getEdgeWeight(e)
      if (srcVertex != desVertex) graph.addEdge(srcVertex, desVertex, weight)
    })
    graph.exportPng("beforeRetiming")
    graph
  }

  /** solve retiming by linear programming on a component graph
   *
   * @return a map from leave components to its retiming value
   */
  def getRetimingValue = {
    val maximumLatency = 500
    implicit val model: MPModel = MPModel(SolverLib.oJSolver)

    // declare vertices as variables
    val vertices = vertexSet().asScala.toSeq
    val variableMap: Map[ScalaLocated, MPFloatVar] = vertices.zipWithIndex.map { case (vertex, i) =>
      vertex -> MPFloatVar(s"r$i", 0, maximumLatency)
    }.toMap

    // the all-in-one target function
    val ins = inputs.toSeq
    val outs = outputs.toSeq
    add(variableMap(inputs.head) := 0) // set value 0 for the first input port
    val coeffs = outs.map(_ => 1.0) ++ ins.map(_ => -1.0)
    val expr = (outs ++ ins).zip(coeffs)
      .map { case (v, coeff) => variableMap(v) * coeff }
      .reduce(_ + _) // \Sigma outs - \Sigma ins

    minimize(expr)

    // constraint 1: inputs/outputs are aligned
    ins.prevAndNext { case (prev, next) => add(variableMap(next) - variableMap(prev) := 0) }
    outs.prevAndNext { case (prev, next) => add(variableMap(next) - variableMap(prev) := 0) }
    // constraint 2: component latency & non-negative edge, regrading every component vertex as the input port
    edgeSet().asScala.foreach { e =>
      val sourceVar = variableMap(getEdgeSource(e))
      val targetVar = variableMap(getEdgeTarget(e))
      if (!inputs.contains(getEdgeSource(e))) {
        val component = getEdgeSource(e).asInstanceOf[ChainsawBaseModule]
        val componentLatency = component.gen.asInstanceOf[FixedLatency].latency()
        val edgeLatency = getEdgeWeight(e)
        add(targetVar - sourceVar >:= componentLatency + edgeLatency)
      }
    }
    // TODO: extra constraint: when a port have multiple driver components, the retiming value of all drivers should be the same

    // solve the LP problem
    start()

    // rounding, toInt is equivalent to ceil, which is not appropriate
    import scala.math.round
    val minValue = variableMap.values.map(_.value.get).min
    val solution = vertices.map { v =>
      val variable = variableMap(v)
      v -> round(variable.value.getOrElse(-1.0) - minValue).toInt
    }.toMap
    if (verbose >= 1) logger.info(
      s"solution:\n\t ${vertices.map(v => s"$v -> ${solution(v)}").mkString("\n\t")}" +
        s"\n\tsolution latency = ${solution(outs.head) - solution(ins.head)}"
    )
    release()

    solution
  }

  /** apply retiming to the module
   *
   * @param top design under retiming
   */
  def doRetiming(top: ChainsawBaseModule): Unit = {
    // FIXME: this will ruined the reg descriptor and lead to synthesis error
    /** --------
     * add registers to datapath
     * -------- */
    val retimingInfo = toComponentGraph.getRetimingValue

    var paddingCount = 0
    vertexSet().asScala.map(_.asInstanceOf[BaseType]) // find padding value vertex by vertex
      .foreach { v =>
        val drivers = incomingEdgesOf(v)
        val paddings = drivers.asScala.map { e =>
          val src = getEdgeSource(e).asInstanceOf[BaseType]
          //          if (src.component == v.component) throw new IllegalArgumentException(s"path inside leaf component appears: $src -> $v")
          //          else {
          //
          //          }
          val srcValue = retimingInfo(if (top.getAllIo.contains(src)) src else src.component)
          val desValue = retimingInfo(if (top.getAllIo.contains(v)) v else v.component)
          val componentLatency =
            if (top.getAllIo.contains(src)) 0
            else src.component.asInstanceOf[ChainsawBaseModule].gen.asInstanceOf[FixedLatency].latency()
          desValue - srcValue - componentLatency - getEdgeWeight(e).toInt // padding value
        }
        require(paddings.forall(padding => padding == paddings.head && padding >= 0), s"padding value of $v is not the same: ${paddings.mkString(" ")}")
        if (paddings.nonEmpty && paddings.head > 0) {
          if (verbose >= 1) logger.info(s"RETIMING: padding ${paddings.head} cycles before $v")
          v.insertDelayBefore(paddings.head, top)
          paddingCount += paddings.head * v.getBitsWidth
        }
      }
    logger.info(s"$paddingCount extra registers added after retiming")
    /** --------
     * add registers to control path
     * -------- */
    val latency = retimingInfo(top.dataOut.head.raw) - retimingInfo(top.dataIn.head.raw)
    top.rework {
      top.validOut.removeAssignments()
      top.validOut := top.validIn.validAfter(latency)
    }
    logger.info(s"latency after retiming =  $latency")
  }
}

object RetimingGraph {

  /** build a retiming graph from a Chainsaw module
   */
  def apply(top: ChainsawBaseModule): RetimingGraph = {

    /** --------
     * get leaves of a component for retiming
     * -------- */
    val leaves = mutable.HashSet[ChainsawBaseModule]()

    def dfs(current: Component): Unit = {
      current.children.foreach { c =>
        val child = c.asInstanceOf[ChainsawBaseModule]
        if (!child.gen.isInstanceOf[OverwriteLatency]) leaves += child
        else {
          if (child.children.isEmpty) throw new IllegalArgumentException(s"component $child is a leaf, but has no fixed latency")
          else dfs(child)
        }
      }
    }

    dfs(top)

    /** --------
     * build a graph containing all I/O of leaves and top
     * -------- */
    val topIos = top.dataIo
    val innerIos = leaves.flatMap(_.dataIo)
    val allIos = topIos ++ innerIos

    val graph = new RetimingGraph
    topIos.filter(_.isInput).foreach(graph.addInput)
    topIos.filter(_.isOutput).foreach(graph.addOutput)
    innerIos.foreach(graph.addVertex)

    // by this filter, only the edges between vertices contained in allIos will be added to the graph
    def retimingFilter(e: Expression) = allIos.contains(e)

    def latencyEval(e: Expression) = {
      e match {
        case baseType: BaseType => if (baseType.isReg) 1.0 else 0.0
        case _ => 0.0
      }
    }

    // targeting at targets, rather than allIos, this prevent the paths inside a leaf component from being added to the graph
    val targets = topIos.filter(_.isOutput) ++ innerIos.filter(_.isInput)

    targets.foreach { des =>
      val sources = des.getSourcesAndDepths(retimingFilter, latencyEval)
      sources.foreach { case (src, d) =>
        graph.addEdge(src, des, d.toInt)
      }
    }
    graph
  }
}