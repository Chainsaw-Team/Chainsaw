package Chainsaw.dfg

class DfgEdge(val delay: Int, val outId: Int, val inId: Int)(implicit dfg: Dfg) {
  override def toString: String = delay.toString // TODO: get name by reflection

  def source = dfg.getEdgeSource(this)
  def target = dfg.getEdgeTarget(this)

}
