package Chainsaw.edaFlow

import Chainsaw._
import Chainsaw.edaFlow.boards.Zybo
import Chainsaw.edaFlow.vivado.VivadoTask
import org.apache.commons.io.FileUtils
import spinal.lib.Timeout
import spinal.core._
import spinal.core.fiber.Handle

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

trait Board {
  val xdcFile:File
  val device:XilinxDevice
  val defaultClockDomain:ClockDomain
}



trait Nexys4Ddr extends Component with Board {

  val CLK100MHZ = in Bool()
  val SW = in Bits(16 bits)
  lazy val JA = out Bits(8 bits)
  lazy val JB = out Bits(8 bits)
  lazy val JC = out Bits(8 bits)
  lazy val JD = out Bits(8 bits)

  lazy val LED = out Bits(16 bits)

  override val xdcFile: File = new File(xdcFileDir, "Nexys-A7-100T-Master.xdc")
  override val device: XilinxDevice = new XilinxDevice(Series7, "XC7A100T-CSG324-1".toLowerCase(), 100 MHz, None) // TODO: budget
  val clockDomainConfig = ClockDomainConfig(clockEdge = RISING, resetKind = BOOT)
  override val defaultClockDomain = new ClockDomain(clock = CLK100MHZ, config = clockDomainConfig, frequency = FixedFrequency(125 MHz))

}

trait Basys3 extends Component with Board {

  val clk = in Bool()
  val sw = in Bits(16 bits)
  val btnC, btnU, btnD, btnL, btnR = in Bool()

  // PMOD
  lazy val JA = out Bits(8 bits)
  lazy val JB = out Bits(8 bits)
  lazy val JC = out Bits(8 bits)
  lazy val JD = out Bits(8 bits)

  // 7-segments
  lazy val led = out Bits(16 bits)
  lazy val seg = out Bits(16 bits)
  lazy val dp = out Bool()
  lazy val an = out Bits(4 bits)

  // VGA
  lazy val vgaRed = out Bits(4 bits)
  lazy val vgaBlue = out Bits(4 bits)
  lazy val vgaGreen = out Bits(4 bits)
  lazy val Hsync, VSync = out Bool()

  // USB-RS232
  val RsRx = in Bool()
  lazy val RsTx = out Bool()

  override val xdcFile: File = new File(xdcFileDir, "Basys3.xdc")
  override val device: XilinxDevice = new XilinxDevice(Series7, " XC7A35T-CPG236-1".toLowerCase(), 100 MHz, None) // TODO: budget
  val clockDomainConfig = ClockDomainConfig(clockEdge = RISING, resetKind = BOOT)
  override val defaultClockDomain = new ClockDomain(clock = clk, config = clockDomainConfig, frequency = FixedFrequency(125 MHz))

}

case class ZyboLed()  extends Zybo{
    defaultClockDomain on {
      val trigger = sw(0).d(3).changed
      val pulseGen = Timeout(5 us)
      when(trigger)(pulseGen.clear())

      val clk_p = RegInit(False)
      clk_p.toggleWhen(True)
      val clk_n = RegNext(clk_p)

      jb_p.asBools.foreach(_ := ~pulseGen.state)
      jb_n.asBools.foreach(_ := False)

      led := sw.d(3)
    }
}

case class Nexys4Led()  extends Nexys4Ddr {
  defaultClockDomain on {
    val trigger = SW(0).d(3).changed
    val pulseGen = Timeout(5 us)
    when(trigger)(pulseGen.clear())

    JC.asBools.foreach(_ := ~pulseGen.state)
//    JC.asBools.foreach(_ := False)

    LED := SW.d(3)
  }
}

case class Basys3Led()  extends Basys3 {
  defaultClockDomain on {
    val trigger = sw(0).d(3).changed
    val pulseGen = Timeout(5 us)
    when(trigger)(pulseGen.clear())

    JA.asBools.foreach(_ := ~pulseGen.state)

    led := sw.d(3)
  }
}

object ZyboLed extends App {
  SpinalVerilog(ZyboLed())
  VivadoTask.genBoardBitStream(Basys3Led(), "Basys3Led")
}

