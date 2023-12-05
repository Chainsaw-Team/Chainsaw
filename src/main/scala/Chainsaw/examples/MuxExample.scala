package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.edaFlow.Device.vu9p
import Chainsaw.edaFlow.vivado._

import java.io.File
import scala.language.postfixOps

case class MuxExample(width: Int) extends Component {
  val select  = in UInt (10 bits)
  val dataIn  = in Vec (UInt(width bits), 1024)
  val dataOut = out UInt (width bits)
  dataOut := dataIn(select)
}

object MuxExample extends App {
  //  VivadoSynth(MuxExample(1), "synthMux1", ChainsawSpinalConfig())
  VivadoTask.synth("MuxExample", MuxExample(8), vu9p, Seq[File](), None, ChainsawSpinalConfig())
}
