package Chainsaw.edaFlow.boards.digilent

import Chainsaw.edaFlow.{Board, Series7, XilinxDevice}
import Chainsaw.xdcFileDir
import spinal.core._

import java.io.File

class Basys3 extends Component with Board {

  val clk                          = in Bool ()
  val sw                           = in Bits (16 bits)
  val btnC, btnU, btnD, btnL, btnR = in Bool ()

  // PMOD
  lazy val JA = out Bits (8 bits)
  lazy val JB = out Bits (8 bits)
  lazy val JC = out Bits (8 bits)
  lazy val JD = out Bits (8 bits)

  // 7-segments
  lazy val led = out Bits (16 bits)
  lazy val seg = out Bits (16 bits)
  lazy val dp  = out Bool ()
  lazy val an  = out Bits (4 bits)

  // VGA
  lazy val vgaRed       = out Bits (4 bits)
  lazy val vgaBlue      = out Bits (4 bits)
  lazy val vgaGreen     = out Bits (4 bits)
  lazy val Hsync, VSync = out Bool ()

  // USB-RS232
  val RsRx      = in Bool ()
  lazy val RsTx = out Bool ()

  override val xdcFile: File = new File(xdcFileDir, "Basys3.xdc")
  override val device: XilinxDevice =
    new XilinxDevice(Series7, " XC7A35T-CPG236-1".toLowerCase(), 100 MHz, None) // TODO: budget
  val clockDomainConfig = ClockDomainConfig(clockEdge = RISING, resetKind = BOOT)
  override val defaultClockDomain =
    new ClockDomain(clock = clk, config = clockDomainConfig, frequency = FixedFrequency(125 MHz))

}
