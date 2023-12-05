package Chainsaw.dfg
import Chainsaw.NumericType
import Chainsaw.arithmetic.floating._
import Chainsaw.edaFlow.Device._
import spinal.core._
import spinal.lib.experimental.math.Floating

import scala.collection.immutable

// floating operators are designed for DFG verification
abstract class FloatingBinaryOperator(implicit dfg: Dfg) extends Operator {

  override def inputTypes: Seq[NumericType]  = Seq.fill(2)(NumericType.Float())
  override def outputTypes: Seq[NumericType] = Seq(NumericType.Float())

  override val delay: Int = 0
}

class FloatingAdd(implicit dfg: Dfg) extends FloatingBinaryOperator {
  override val name: String          = "FloatingAdd"
  override val executionTime: Double = 1.0

  override def implFloating(inputs: Seq[Floating]): Seq[Floating] = {
    val op = SinglePrecisionFPAdd(UltraScale, 10 MHz)
    op.x := inputs(0)
    op.y := inputs(1)
    Seq(op.z)
  }
}

class FloatingSub(implicit dfg: Dfg) extends FloatingBinaryOperator {
  override val name: String          = "FloatingSub"
  override val executionTime: Double = 1.0

  override def implFloating(inputs: Seq[Floating]): Seq[Floating] = {
    val op = SinglePrecisionFPSub(UltraScale, 10 MHz)
    op.x := inputs(0)
    op.y := inputs(1)
    Seq(op.z)
  }
}

class FloatingMult(implicit dfg: Dfg) extends FloatingBinaryOperator {
  override val name: String          = "FloatingMult"
  override val executionTime: Double = 5.0

  override def implFloating(inputs: Seq[Floating]): Seq[Floating] = {
    val op = SinglePrecisionFPMult(UltraScale, 10 MHz)
    op.x := inputs(0)
    op.y := inputs(1)
    Seq(op.z)
  }
}

class FloatingDiv(implicit dfg: Dfg) extends FloatingBinaryOperator {
  override val name: String          = "FloatingDiv"
  override val executionTime: Double = 20.0

  override def implFloating(inputs: Seq[Floating]): Seq[Floating] = {
    val op = SinglePrecisonFPDiv(UltraScale, 10 MHz)
    op.x := inputs(0)
    op.y := inputs(1)
    Seq(op.z)
  }
}

class Switch2(implicit dfg: Dfg) extends Operator {

  override def inputTypes: Seq[NumericType]  = NumericType.Bool() +: Seq.fill(2)(NumericType.Float())
  override def outputTypes: Seq[NumericType] = Seq.fill(2)(NumericType.Float())
  override val delay: Int                    = 0

  override val name: String          = "Switch2"
  override val executionTime: Double = 2.0

  override def implFloating(inputs: Seq[Floating]): Seq[Floating] = {
    val switch = inputs(0).asBits.asBool
    Seq(
      Mux(switch, inputs(2), inputs(1)),
      Mux(switch, inputs(1), inputs(2))
    )
  }
}

class Mux2(implicit dfg: Dfg) extends Operator {

  override def inputTypes: Seq[NumericType]  = NumericType.Bool() +: Seq.fill(2)(NumericType.Float())
  override def outputTypes: Seq[NumericType] = Seq.fill(2)(NumericType.Float())
  override val delay: Int                    = 0

  override val name: String          = "Mux2"
  override val executionTime: Double = 2.0

  override def implFloating(inputs: Seq[Floating]): Seq[Floating] = {
    val switch = inputs(0).asBits.asBool
    Seq(Mux(switch, inputs(1), inputs(2)))
  }
}

// by import FloatingOperators._, one can use the operators above inside a DSP Area
object FloatingOperators {

  def draw(operator: Operator, inputs: Seq[Signal])(implicit dfg: Dfg): Seq[Signal] = {
    inputs.zipWithIndex.foreach { case (signal, id) =>
      val e = new DfgEdge(delay = 0, signal.outId, id)
      dfg.addEdge(signal.v, operator, e)
    }
    (0 until operator.outCount).map(id => Signal(operator, id))
  }
  implicit class FloatingOps(signal: Signal) {

    def binaryOp(operator: DfgVertex, that: Signal)(implicit dfg: Dfg): Signal = {
      val e0 = new DfgEdge(0, signal.outId, 0) // TODO: merge existing delay
      val e1 = new DfgEdge(0, that.outId, 1)
      dfg.addEdge(signal.v, operator, e0)
      dfg.addEdge(that.v, operator, e1)
      Signal(operator, 0)
    }

    def +(that: Signal)(implicit dfg: Dfg): Signal = binaryOp(new FloatingAdd, that)

    def -(that: Signal)(implicit dfg: Dfg): Signal = binaryOp(new FloatingSub, that)

    def *(that: Signal)(implicit dfg: Dfg): Signal = binaryOp(new FloatingMult, that)

    def *(that: Float)(implicit dfg: Dfg): Signal = *(Signal(new Constant(that, NumericType.Float()), 0))

    def /(that: Signal)(implicit dfg: Dfg): Signal = binaryOp(new FloatingDiv, that)
  }

  def Mux(switch: Signal, a: Signal, b: Signal)(implicit dfg: Dfg): Signal = draw(new Mux2, Seq(switch, a, b)).head

  def Switch(switch: Signal, a: Signal, b: Signal)(implicit dfg: Dfg): (Signal, Signal) = {
    val Seq(c, d) = draw(new Switch2, Seq(switch, a, b))
    (c, d)
  }

}
