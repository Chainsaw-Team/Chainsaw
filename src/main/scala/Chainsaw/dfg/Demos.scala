package Chainsaw.dfg

import Chainsaw.dfg.FloatingOperators._
import spinal.core._
import spinal.lib._
import spinal.lib.experimental.math.Floating

case class DfgUnderTest(id: Int) extends Dfg {

  val i: Signal = SIn()
  val o: Signal = SOut()

  id match {
    case 0 => // adder
      o := i + i
    case 1 => // systolic FIR - running sum
      val coeffs: Seq[Float]  = Seq.fill(6)(1.0.toFloat)
      val delayLine           = Seq.iterate(i, coeffs.length)(_.d(2))
      val scaled: Seq[Signal] = delayLine.zip(coeffs).map { case (signal, coeff) => signal * coeff }
      o := scaled.reduce((a: Signal, b: Signal) => (a + b).d())
    case 2 => // adder graph for constant multiplication

  }
}

object FunctionUnderTest {
  def apply(id: Int, input: Seq[Float]): Seq[Float] = {
    id match {
      case 0 => // adder
        Seq(input.head * 2)

    }
  }
}

object DfgUnderTest extends App {

  // instantiation
  val testId                           = 0
  def getDut                           = DfgUnderTest(testId)
  def getRet: Seq[Float] => Seq[Float] = FunctionUnderTest(testId, _)

  SpinalConfig().generateVerilog {
    new Component {
      val dut = getDut

      val dataIn  = in(Floating(8, 23))
      val dataOut = out(Floating(8, 23))

      dut.i.floating := dataIn
      dataOut        := dut.o.floating

      dut.build()
      dut.exportDrawIo("dutGraph")
    }
  }

  SpinalConfig().generateVerilog {
    new Component {
      val dut = getDut

      val dataIn  = slave(Stream(Floating(8, 23)))
      val dataOut = master(Stream(Floating(8, 23)))

      dataIn               >> dut.i.floatingStream
      dut.o.floatingStream >> dataOut

      dut.useStream = true
      dut.build()
    }
  }

  // simulation

  val stimulus = Seq.fill(20)(Seq(1.0f))
  val golden   = stimulus.map(getRet)

  DfgStreamTest(getDut, stimulus, golden)

}
