package Chainsaw.dag

import scala.collection.JavaConverters._ // as JGraphT is based on Java

import Chainsaw._
import Chainsaw.xilinx._

class DagVertex(val gen: ChainsawGenerator)(implicit ref: Dag) {

  import gen._

  ref.addVertex(this)

  // TODO: without this, all I/O vertices have the same hashCode, but why?
  override def hashCode(): Int = super.hashCode()

  override def equals(obj: Any): Boolean = this.hashCode() == obj.hashCode()

  var vertexName = s"${gen.name}_${ref.vertexSet().size()}"

  // generate DagPorts
  def in(portOrder: Int): DagPort = { // get one port
    assert(portOrder < inputWidths.length)
    DagPort(this, portOrder, In)
  }

  def out(portOrder: Int): DagPort = {
    assert(portOrder < outputWidths.length)
    DagPort(this, portOrder, Out)
  }

  /** --------
   * methods for connections
   * -------- */
  def :=(ports: DagPort*)(implicit ref: Dag): Unit = {
    require(this.inCount == ports.length, "partial connection by := is forbidden as it is dangerous")
    //    require(inputWidths.zip(ports).forall{ case (width, port) => width == port.width},
    //      s"\nwidth mismatch at vertex $vertexName: \nsource:${ports.map(_.width).mkString(" ")}, target: ${inputWidths.mkString(" ")}")
    ports.zip(inPorts).foreach { case (port, inPort) => ref.addEdge(port, inPort) }
  }

  def <<(source: DagVertex)(implicit ref: Dag) = {
    :=(source.outPorts: _*)
    this
  }

  def >>(target: DagVertex)(implicit ref: Dag) = target << this

  /** --------
   * methods for accessing neighbors
   * -------- */
  def inPorts: Seq[DagPort] = inputWidths.indices.map(in) // get all ports

  def outPorts: Seq[DagPort] = outputWidths.indices.map(out)

  // number of ports
  def inCount = inputWidths.length

  def outCount = outputWidths.length

  def inDegree(implicit ref: Dag): Int = ref.inDegreeOf(this)

  def outDegree(implicit ref: Dag): Int = ref.outDegreeOf(this)

  /** disordered incoming edges
   */
  def incomingEdges(implicit ref: Dag): Seq[DagEdge] =
    ref.incomingEdgesOf(this).asScala.toSeq

  /** ordered source ports
   */
  def sourcePorts(implicit ref: Dag): Seq[DagPort] =
    this.incomingEdges.sortBy(_.inOrder)
      .map(e => DagPort(ref.getEdgeSource(e), e.outOrder, Out))

  /** ordered source vertices
   */
  def sources(implicit ref: Dag): Seq[DagVertex] =
    this.incomingEdges.sortBy(_.inOrder)
      .map(ref.getEdgeSource)

  /** disordered outgoing edges
   */
  def outgoingEdges(implicit ref: Dag): Seq[DagEdge] =
    ref.outgoingEdgesOf(this).asScala.toSeq

  /** ordered target ports
   */
  def targetPorts(implicit ref: Dag): Seq[DagPort] =
    this.outgoingEdges.sortBy(_.outOrder)
      .map(e => DagPort(ref.getEdgeTarget(e), e.inOrder, In))

  /** ordered target vertices
   */
  def targets(implicit ref: Dag): Seq[DagVertex] =
    this.outgoingEdges.sortBy(_.outOrder)
      .map(ref.getEdgeTarget)

  // I/O attribute
  def isInput(implicit ref: Dag) = ref.inputs.contains(this)

  def isOutput(implicit ref: Dag) = ref.outputs.contains(this)

  def isIo(implicit ref: Dag) = isInput || isOutput

  def setName(name: String): Unit = vertexName = name

  override def toString = s"$vertexName"

  def cloneTo(targetGraph: Dag): DagVertex = {
    val ret = new DagVertex(gen)(targetGraph)
    ret.setName(vertexName)
    ret
  }
}

object DagVertex {
  def apply(gen: ChainsawGenerator)(implicit ref: Dag): DagVertex = new DagVertex(gen)(ref)
}
