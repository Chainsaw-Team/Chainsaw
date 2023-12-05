package Chainsaw.edaFlow.boards.digilent

import Chainsaw.edaFlow.{Board, Series7, XilinxDevice}
import Chainsaw.xdcFileDir
import spinal.core._

import java.io.File

class Zybo extends Component with Board {

  val clk  = in Bool ()
  val sw   = in Bits (4 bits)
  val jc_p = in Bits (4 bits)
  val jc_n = in Bits (4 bits)

  // outputs
  lazy val jb_p = out Bits (4 bits)
  lazy val jb_n = out Bits (4 bits)

  lazy val led = out Bits (4 bits)

  override val xdcFile: File        = new File(xdcFileDir, "Zybo.xdc")
  override val device: XilinxDevice = new XilinxDevice(Series7, "xc7z010clg400-1", 125 MHz, None) // TODO: budget
  val clockDomainConfig             = ClockDomainConfig(clockEdge = RISING, resetKind = BOOT)
  override val defaultClockDomain =
    new ClockDomain(clock = clk, config = clockDomainConfig, frequency = FixedFrequency(125 MHz))

}
