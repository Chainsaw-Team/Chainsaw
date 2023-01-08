package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.xilinx._

import scala.language.postfixOps

case class MuxExample(width: Int) extends Component {
  val select = in UInt (10 bits)
  val dataIn = in Vec(UInt(width bits), 1024)
  val dataOut = out UInt (width bits)
  dataOut := dataIn(select)
}

object MuxExample extends App {
  //  VivadoSynth(MuxExample(1), "synthMux1", ChainsawSpinalConfig())
  VivadoSynth(MuxExample(8), "synthMux8", ChainsawSpinalConfig())
}
