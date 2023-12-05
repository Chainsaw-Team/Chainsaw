package Chainsaw.edaFlow.boards

import Chainsaw.edaFlow.Device._
import Chainsaw._
import Chainsaw.edaFlow._
import spinal.core._
import spinal.lib.master

import java.io.File
import scala.language.postfixOps

case class IBUFDS() extends BlackBox {
  val I  = in Bool ()
  val IB = in Bool ()
  val O  = out Bool ()
}

object IBUFDS {
  def Lvds2Clk(p: Bool, n: Bool): Bool = {
    val buf = IBUFDS()
    buf.I  := p
    buf.IB := n
    buf.O
  }
}

class Z7P extends Component with Board {
  override val xdcFile: File                   = ???
  override val device: XilinxDevice            = ???
  override val defaultClockDomain: ClockDomain = ???
}

class AXKU041 extends Component with Board {

  val sys_clk_p, sys_clk_n, rst_n = in Bool ()
  val user_key                    = in Bool ()
  lazy val pcie                   = master(Pcie(8))

  // FMC
  lazy val FMC1_LPC, FMC2_LPC = FmcLpc()

  def useFmc1(asMaster: Boolean): Unit = {
    if (asMaster) FMC1_LPC.asMaster() else FMC1_LPC.asSlave()
  }
  def useFmc2(asMaster: Boolean, dataOnly: Boolean): Unit = {
    if (asMaster) FMC2_LPC.asMaster() else FMC2_LPC.asSlave()
    if (!dataOnly) FMC2_LPC.asCarrier()
  }

  // SMA
  lazy val SMA_CLKIN_P, SMA_CLKIN_N = out Bool ()

  override val xdcFile: File = new File(xdcFileDir, "AXKU041.xdc")
  override val device: XilinxDevice =
    new XilinxDevice(UltraScalePlus, "XCKU040-FFVA1156-2-i".toLowerCase(), 200 MHz, None)

  // clock domain
  val clk = Bool()
  clk := IBUFDS.Lvds2Clk(sys_clk_p, sys_clk_n)
  val clockDomainConfig = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW)
  override val defaultClockDomain =
    new ClockDomain(clock = clk, reset = rst_n, config = clockDomainConfig, frequency = FixedFrequency(200 MHz))

}
