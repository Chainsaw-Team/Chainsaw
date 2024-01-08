package Chainsaw.edaFlow.boards.digilent

import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow._
import spinal.core._

import java.io.File

class Nexys4Ddr extends Component with Board {

  val CLK100MHZ = in Bool ()
  val SW        = in Bits (16 bits)
  lazy val JA   = out Bits (8 bits)
  lazy val JB   = out Bits (8 bits)
  lazy val JC   = out Bits (8 bits)
  lazy val JD   = out Bits (8 bits)

  lazy val LED = out Bits (16 bits)

  override val xdcFile: File = new File(xdcFileDir, "Nexys-A7-100T-Master.xdc")
  override val device: XilinxDevice =
    new XilinxDevice(Series7, "XC7A100T-CSG324-1".toLowerCase(), 125 MHz, None) // TODO: budget
  val clockDomainConfig = ClockDomainConfig(clockEdge = RISING, resetKind = BOOT)
  override val defaultClockDomain =
    new ClockDomain(clock = CLK100MHZ, config = clockDomainConfig, frequency = FixedFrequency(125 MHz))

}
