package Chainsaw.dfg
import Chainsaw.NumericType

case class Signal(v: DfgVertex, outId: Int) {
  def :=(src: Signal)(implicit dfg: Dfg) = {
    val e = new DfgEdge(0, src.outId, 0)
    dfg.addEdge(src.v, v, e)
  }

  def d(delay: Int = 1)(implicit dfg: Dfg) = {
    val e    = new DfgEdge(delay, outId, 0)
    val noOp = new Intermediate(v.outputTypes.head)
    dfg.addEdge(v, noOp, e)
    Signal(noOp, 0)
  }

  def floating = this.v.asInstanceOf[Io].floating
  def fixed    = this.v.asInstanceOf[Io].fixed
}

object S {
  def apply(dataType: NumericType = NumericType.Float())(implicit dfg: Dfg): Signal = Signal(new NoOp(dataType), 0)
}

object SIn {
  def apply(dataType: NumericType = NumericType.Float())(implicit dfg: Dfg): Signal = {
    val input = new Input(dataType)
    Signal(input, 0)
  }
}

object SOut {
  def apply(dataType: NumericType = NumericType.Float())(implicit dfg: Dfg): Signal = {
    val output = new Output(dataType)
    Signal(output, 0)
  }
}
