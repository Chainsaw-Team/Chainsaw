package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.xilinx._
import Chainsaw.device._

import scala.language.postfixOps

// show how stupid Vivado is!
case class InverseExample(method: Int) extends Component {
  val dataIn = in Bits (500 bits)
  val dataOut = out Bits (500 bits)
  method match {
    case 0 => dataOut := ~dataIn
    case 1 => val lutGen = LUT5to2(
      (i0, i1, i2, i3, i4) => !i0,
      (i0, i1, i2, i3, i4) => !i1)
      (0 until 250).foreach { i =>
        val Seq(even, odd) = lutGen.process(dataIn(i * 2), dataIn(i * 2 + 1), False, False, False)
        dataOut(i * 2) := even
        dataOut(i * 2 + 1) := odd
      }
  }
}

object InverseExample {
  def main(args: Array[String]): Unit = {
    VivadoSynth(InverseExample(0), "InverseExampleBetter") // 500 LUT
    VivadoSynth(InverseExample(1), "InverseExampleBetter") // 250 LUT
  }
}
