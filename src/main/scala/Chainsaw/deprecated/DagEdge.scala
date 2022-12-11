package Chainsaw.deprecated

import spinal.core.Data

class DagEdge(val inOrder: Int, val outOrder: Int)(implicit ref: Dag) {

  def source(implicit ref: Dag): DagVertex = ref.getEdgeSource(this)

  def sourcePort(implicit ref: Dag) = ref.getEdgeSource(this).out(outOrder)

  def target(implicit ref: Dag): DagVertex = ref.getEdgeTarget(this)

  def targetPort[THard <: Data](implicit ref: Dag) = ref.getEdgeTarget(this).in(inOrder)

  def weight[THard <: Data](implicit ref: Dag): Double = ref.getEdgeWeight(this)

  def toStringInGraph[THard <: Data](implicit ref: Dag): String =
    s"$source -> $weight -> $target"

  override def toString = weight.toString
}
