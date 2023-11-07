package Chainsaw.edaFlow

import Chainsaw._
import Chainsaw.edaFlow.vivado.VivadoTask
import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.core.fiber.Handle

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Board {

  sealed trait Board extends Module

  case class NexysPinConfig(
      useSwitch: Boolean            = false,
      useLed: Boolean               = false,
      useRGB: Boolean               = false,
      useSegDisplay: Boolean        = false,
      useCpuReset: Boolean          = false,
      useButton: Boolean            = false,
      usePmodHeaderA: Boolean       = false,
      usePmodHeaderB: Boolean       = false,
      usePmodHeaderC: Boolean       = false,
      usePmodHeaderD: Boolean       = false,
      usePmodJXADC: Boolean         = false,
      useVGA: Boolean               = false,
      useMicroSDConnector: Boolean  = false,
      useAccelerometer: Boolean     = false,
      useTemperatureSensor: Boolean = false,
      useMicrophone: Boolean        = false,
      usePWMAudioAmplifier: Boolean = false,
      useUsbRS232: Boolean          = false,
      useUSBHid: Boolean            = false,
      useEthernet: Boolean          = false,
      useSPIFlash: Boolean          = false
  ) {
    def getMatchWords: Seq[(Boolean, String)] = {
      val matchWords = ArrayBuffer[(Boolean, String)]()
      matchWords.append(useSwitch            -> "Switches")
      matchWords.append(useLed               -> s"LEDs")
      matchWords.append(useRGB               -> s"RGB LEDs")
      matchWords.append(useSegDisplay        -> s"7 segment display")
      matchWords.append(useCpuReset          -> s"CPU Reset")
      matchWords.append(useButton            -> s"Buttons")
      matchWords.append(usePmodHeaderA       -> s"Pmod Header JA")
      matchWords.append(usePmodHeaderB       -> s"Pmod Header JB")
      matchWords.append(usePmodHeaderC       -> s"Pmod Header JC")
      matchWords.append(usePmodHeaderD       -> s"Pmod Header JD")
      matchWords.append(usePmodJXADC         -> s"mod Header JXADC")
      matchWords.append(useVGA               -> s"VGA Connector")
      matchWords.append(useMicroSDConnector  -> s"Micro SD Connector")
      matchWords.append(useAccelerometer     -> s"Accelerometer")
      matchWords.append(useTemperatureSensor -> s"Temperature Sensor")
      matchWords.append(useMicrophone        -> s"Omnidirectional Microphone")
      matchWords.append(usePWMAudioAmplifier -> s"PWM Audio Amplifier")
      matchWords.append(useUsbRS232          -> s"USB-RS232 Interface")
      matchWords.append(useUSBHid            -> s"USB HID")
      matchWords.append(useEthernet          -> s"SMSC Ethernet")
      matchWords.append(useSPIFlash          -> s"Quad SPI Flash")
      matchWords
    }
  }
  abstract class Nexys4A7T100(pinConfig: NexysPinConfig) extends Board {

    val CLK100MHZ    = in Bool ()
    val SW           = pinConfig.useSwitch generate (in Bits (16 bits))
    val LED          = pinConfig.useLed generate (out Bits (16 bits))
    val LED16_B      = pinConfig.useRGB generate (out Bool ())
    val LED16_G      = pinConfig.useRGB generate (out Bool ())
    val LED16_R      = pinConfig.useRGB generate (out Bool ())
    val LED17_B      = pinConfig.useRGB generate (out Bool ())
    val LED17_G      = pinConfig.useRGB generate (out Bool ())
    val LED17_R      = pinConfig.useRGB generate (out Bool ())
    val CA           = pinConfig.useSegDisplay generate (out Bool ())
    val CB           = pinConfig.useSegDisplay generate (out Bool ())
    val CC           = pinConfig.useSegDisplay generate (out Bool ())
    val CD           = pinConfig.useSegDisplay generate (out Bool ())
    val CE           = pinConfig.useSegDisplay generate (out Bool ())
    val CF           = pinConfig.useSegDisplay generate (out Bool ())
    val CG           = pinConfig.useSegDisplay generate (out Bool ())
    val DP           = pinConfig.useSegDisplay generate (out Bool ())
    val AN           = pinConfig.useSegDisplay generate (out Bits (8 bits))
    val CPU_RESETN   = pinConfig.useCpuReset generate (in Bool ())
    val BTNC         = pinConfig.useButton generate (in Bool ())
    val BTNU         = pinConfig.useButton generate (in Bool ())
    val BTNL         = pinConfig.useButton generate (in Bool ())
    val BTNR         = pinConfig.useButton generate (in Bool ())
    val BTND         = pinConfig.useButton generate (in Bool ())
    val JA           = pinConfig.usePmodHeaderA generate (in Bits (8 bits))
    val JB           = pinConfig.usePmodHeaderB generate (in Bits (8 bits))
    val JC           = pinConfig.usePmodHeaderC generate (in Bits (8 bits))
    val JD           = pinConfig.usePmodHeaderD generate (in Bits (8 bits))
    val XA_N         = pinConfig.usePmodJXADC generate (in Bits (4 bits))
    val XA_P         = pinConfig.usePmodJXADC generate (in Bits (4 bits))
    val VGA_R        = pinConfig.useVGA generate (in Bits (4 bits))
    val VGA_G        = pinConfig.useVGA generate (in Bits (4 bits))
    val VGA_B        = pinConfig.useVGA generate (in Bits (4 bits))
    val VGA_HS       = pinConfig.useVGA generate (in Bool ())
    val VGA_VS       = pinConfig.useVGA generate (in Bool ())
    val SD_RESET     = pinConfig.useMicroSDConnector generate (in Bool ())
    val SD_CD        = pinConfig.useMicroSDConnector generate (in Bool ())
    val SD_SCK       = pinConfig.useMicroSDConnector generate (in Bool ())
    val SD_CMD       = pinConfig.useMicroSDConnector generate (in Bool ())
    val SD_DAT       = pinConfig.useMicroSDConnector generate (in Bits (4 bits))
    val ACL_MISO     = pinConfig.useAccelerometer generate (in Bool ())
    val ACL_MOSI     = pinConfig.useAccelerometer generate (in Bool ())
    val ACL_SCLK     = pinConfig.useAccelerometer generate (in Bool ())
    val ACL_CSN      = pinConfig.useAccelerometer generate (in Bool ())
    val ACL_INT      = pinConfig.useAccelerometer generate (in Bits (2 bits))
    val TMP_SCL      = pinConfig.useTemperatureSensor generate (in Bool ())
    val TMP_SDA      = pinConfig.useTemperatureSensor generate (in Bool ())
    val TMP_INT      = pinConfig.useTemperatureSensor generate (in Bool ())
    val TMP_CT       = pinConfig.useTemperatureSensor generate (in Bool ())
    val M_CLK        = pinConfig.useMicrophone generate (in Bool ())
    val M_DATA       = pinConfig.useMicrophone generate (in Bool ())
    val M_LRSEL      = pinConfig.useMicrophone generate (in Bool ())
    val AUD_PWM      = pinConfig.usePWMAudioAmplifier generate (out Bool ())
    val AUD_SD       = pinConfig.usePWMAudioAmplifier generate (out Bool ())
    val UART_TXD_IN  = pinConfig.useUsbRS232 generate (in Bool ())
    val UART_RXD_OUT = pinConfig.useUsbRS232 generate (out Bool ())
    val UART_CTS     = pinConfig.useUsbRS232 generate (in Bool ())
    val UART_RTS     = pinConfig.useUsbRS232 generate (out Bool ())
    val PS2_CLK      = pinConfig.useUSBHid generate (in Bool ())
    val PS2_DATA     = pinConfig.useUSBHid generate (in Bool ())
    val ETH_MDC      = pinConfig.useEthernet generate (in Bool ())
    val ETH_MDIO     = pinConfig.useEthernet generate (in Bool ())
    val ETH_RSTN     = pinConfig.useEthernet generate (in Bool ())
    val ETH_CRSDV    = pinConfig.useEthernet generate (in Bool ())
    val ETH_RXERR    = pinConfig.useEthernet generate (in Bool ())
    val ETH_RXD      = pinConfig.useEthernet generate (in Bits (2 bits))
    val ETH_TXEN     = pinConfig.useEthernet generate (out Bool ())
    val ETH_TXD      = pinConfig.useEthernet generate (out Bits (2 bits))
    val ETH_REFCLK   = pinConfig.useEthernet generate (in Bool ())
    val ETH_INTN     = pinConfig.useEthernet generate (in Bool ())

    val Nexys4ClockDomain = ClockDomain(clock = CLK100MHZ, config = xilinxDefaultCDConfig)

    val xdcFile                  = new File(xdcFileDir, s"NexysA7100T/Nexys-A7-100T-Master.xdc")
    val xdcFileAfterActivatePins = new File(xdcFileDir, s"NexysA7100T/NexysA7100T-activatedPins.xdc")

    def getXdcLines(xdcFile: File) = {
      val xdcSource = Source.fromFile(xdcFile)
      val xdcLines  = xdcSource.getLines()
      val lines     = ArrayBuffer[String]()
      while (xdcLines.hasNext) {
        lines += xdcLines.next()
      }
      lines
    }

    val xdcLines = getXdcLines(xdcFile)

    val xdcHeaderPart = xdcLines.slice(0, 5).mkString("\n")

    val xdcClkPart = xdcLines.slice(5, 8).map(_.drop(1)).mkString("\n")

    val matchedIndex = pinConfig.getMatchWords.map(_._2).map { word =>
      val start = xdcLines.indexWhere(s => s.startsWith("##") && s.contains(word))
      val index = xdcLines.indexWhere(s => s.startsWith("##"), start + 1)
      val end   = if (index != -1) index else xdcLines.length
      Tuple2(start, end)
    }

    val xdcFileStringBuffer = ArrayBuffer[String](xdcHeaderPart, xdcClkPart)

    pinConfig.getMatchWords.map(_._1).zip(matchedIndex).foreach { case (isUsed, (start, end)) =>
      val matchedLines   = xdcLines.slice(start, end)
      val activatedLines = if (isUsed) matchedLines.map(_.drop(1)) else matchedLines
      xdcFileStringBuffer += activatedLines.mkString("\n")
    }

    if (xdcFileAfterActivatePins.exists()) xdcFileAfterActivatePins.delete()
    FileUtils.write(xdcFileAfterActivatePins, xdcFileStringBuffer.mkString("\n"))

  }

}

object GenNexys4Board extends App {
  import Board._
  case class TestNexys4Board(nexysPinConfig: NexysPinConfig) extends Nexys4A7T100(nexysPinConfig) {
    Nexys4ClockDomain on {
      LED          := SW.d(1)
      LED16_R      := False
      LED16_G      := False
      LED16_B      := False
      LED17_R      := False
      LED17_G      := False
      LED17_B      := False
      CA           := False
      CB           := False
      CC           := False
      CD           := False
      CE           := False
      CF           := False
      CG           := False
      DP           := False
      AN           := B(0)
      AUD_PWM      := False
      AUD_SD       := False
      UART_RXD_OUT := False
      UART_RTS     := False
      ETH_TXEN     := False
      ETH_TXD      := B(0)
    }

  }
//  SpinalVerilog(
//    TestNexys4Board(
//      NexysPinConfig(
//        useSwitch     = true,
//        useLed        = true,
//        useRGB        = true,
//        useSegDisplay = true,
//        useCpuReset   = true,
//        useButton     = true,
//        usePmodHeaderA = true,
//        usePmodHeaderB = true,
//        usePmodHeaderC = true,
//        usePmodHeaderD = true
//      )
//    )
//  )
  VivadoTask.genModuleBitStream(
    TestNexys4Board(
      NexysPinConfig(
        useSwitch            = true,
        useLed               = true,
        useRGB               = true,
        useSegDisplay        = true,
        useCpuReset          = true,
        useButton            = true,
        usePmodHeaderA       = true,
        usePmodHeaderB       = true,
        usePmodHeaderC       = true,
        usePmodHeaderD       = true,
        usePmodJXADC         = true,
        useVGA               = true,
        useMicroSDConnector  = true,
        useAccelerometer     = true,
        useTemperatureSensor = true,
        useMicrophone        = true,
        usePWMAudioAmplifier = true,
        useUsbRS232          = true,
        useUSBHid            = true,
        useEthernet          = true,
        useSPIFlash          = true
      )
    ),
    "TestNexys4Board",
    a7100t
  )
}
