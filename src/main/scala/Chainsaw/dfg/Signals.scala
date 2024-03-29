package Chainsaw.dfg
import Chainsaw.NumericType
import spinal.core.AFix
import spinal.lib
import spinal.lib.experimental.math.Floating

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

  // TODO: floating stream etc.

  def floating: Floating                   = this.v.asInstanceOf[Io].floating
  def floatingStream: lib.Stream[Floating] = this.v.asInstanceOf[Io].floatingStream
  def fixed: AFix                          = this.v.asInstanceOf[Io].fixed
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
