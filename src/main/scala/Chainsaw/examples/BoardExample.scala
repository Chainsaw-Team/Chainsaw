package Chainsaw.examples

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import java.io.File
import scala.language.postfixOps

trait Nexys4A7T100 extends Module {

  val CLK100MHZ       = in Bool ()
  val mainClockDomain = new ClockDomain(clock = CLK100MHZ, config = xilinxCDConfig)

  val SW  = in Bits (16 bits)
  val LED = out Bits (16 bits)
}

case class PassThrough() extends Nexys4A7T100 {
  new ClockingArea(mainClockDomain) {
    LED := ~SW.d(3)
  }
}

object TestBoard extends App {
  VivadoBin(PassThrough(), "PassThrough", a7100t, new File("src/main/resources/xdc/Nexys4A7100T.xdc"))
}
