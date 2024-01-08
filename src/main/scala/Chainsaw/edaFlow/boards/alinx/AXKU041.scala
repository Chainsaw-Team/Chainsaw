package Chainsaw.edaFlow.boards.alinx

import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow._
import Chainsaw.edaFlow.boards.{FmcLpc, PcieXilinx}
import Chainsaw.primitives.IBUFDS
import spinal.core._
import spinal.lib.{master, slave}
import spinal.lib.com.uart._

import java.io.File
import scala.language.postfixOps

/** ALINX AXKU041 development board
  * @see
  *   [[https://alinx.com/detail/275]] for sales information and manual
  */
class AXKU041 extends Component with Board {

  // pins with fixed direction
  lazy val sys_clk_p, sys_clk_n, rst_n = in Bool ()
  lazy val user_key                    = in Bool ()
  lazy val pcie                        = slave(PcieXilinx(8)) // PCIE
  lazy val UART                        = master(Uart())       // FMC-HPC
  lazy val led_test                    = out Bits (2 bits)
  lazy val led                         = out Bits (4 bits)

  // pins without fixed direction
  lazy val FMC1_LPC, FMC2_LPC       = FmcLpc() // FMC-LPC
  lazy val SMA_CLKIN_P, SMA_CLKIN_N = Bool()   // SMA

  // enable pins
  def useFmc1(asMaster: Boolean): Unit = { // Fmc1 of AXKU401 has no clock I/O
    if (asMaster) FMC1_LPC.asMaster() else FMC1_LPC.asSlave()
  }
  def useFmc2(asMaster: Boolean, dataOnly: Boolean): Unit = {
    if (asMaster) FMC2_LPC.asMaster() else FMC2_LPC.asSlave()
    if (!dataOnly) FMC2_LPC.asCarrier()
  }

  // board definition
  override val xdcFile: File = new File(xdcFileDir, "AXKU041.xdc")
  override val device: XilinxDevice =
    new XilinxDevice(UltraScale, "XCKU040-FFVA1156-2-i".toLowerCase(), 200 MHz, None)

  override lazy val defaultClockDomain = {
    val clk: Bool = Bool()
    clk := IBUFDS.Lvds2Clk(sys_clk_p, sys_clk_n) // LVDS CLK -> single ended clk
    val clockDomainConfig: ClockDomainConfig =
      ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW)
    new ClockDomain(clock = clk, reset = rst_n, config = clockDomainConfig, frequency = FixedFrequency(200 MHz))
  }

}
