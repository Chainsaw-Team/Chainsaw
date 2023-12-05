package Chainsaw.edaFlow.boards.alinx

import Chainsaw.edaFlow.boards.{FmcHpc, PcieXilinx}
import Chainsaw.edaFlow.{Board, UltraScalePlus, XilinxDevice}
import Chainsaw.primitives.IBUFDS
import Chainsaw.xdcFileDir
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._
import spinal.lib.com.i2c.I2c
import spinal.lib.com.uart.Uart

import java.io.File


/** ALINX Z7-P development board
 * @see [[https://alinx.com/detail/593]] for sales information and manual
 */
class Z7P extends Component with Board {

  val sys_clk_p, sys_clk_n = in Bool ()
  lazy val pcie = slave(PcieXilinx(4))
  lazy val FMC = FmcHpc()
  lazy val alinx40Pin = Alinx40Pin()

  lazy val psUart, plUart = master(Uart())
  lazy val ps_led, pl_led = out(Bool())
  lazy val ps_key_n, pl_key_n = in(Bool()) // user key
  lazy val i2c = master(I2c())

  override val xdcFile: File                   = new File(xdcFileDir, "Z7P.xdc")
  override val device: XilinxDevice            = new XilinxDevice(UltraScalePlus, "XCZU7EV-FFVC1156-2-i".toLowerCase(), 200 MHz, None)

  // TODO: M.2
  // TODO: DP
  // TODO: USB3.0
  // TODO: ETH
  // TODO: MicroSD
  // TODO: JTAG
  // TODO: I2C <-> LM75,EEPROM


  // clock domain
  val clk = Bool()
  clk := IBUFDS.Lvds2Clk(sys_clk_p, sys_clk_n)
  val clockDomainConfig = ClockDomainConfig(clockEdge = RISING, resetKind = BOOT)
  override val defaultClockDomain =
    new ClockDomain(clock = clk, config = clockDomainConfig, frequency = FixedFrequency(200 MHz))


}
