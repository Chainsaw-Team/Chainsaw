package Chainsaw.dfg

class Dfg {

  implicit val background: Dfg = this

}

class DfgVertex(implicit dfg: Dfg) {}

class DfgEdge(implicit dfg: Dfg) {}
