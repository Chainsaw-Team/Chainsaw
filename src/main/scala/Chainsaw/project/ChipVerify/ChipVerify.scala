package Chainsaw.project.ChipVerify

import Chainsaw.DataUtil
import Chainsaw.edaFlow.PythonHeaderGenerator
import Chainsaw.edaFlow.boards.alinx._
import Chainsaw.xillybus._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.{AccessType, HtmlGenerator}

import scala.language.postfixOps

/** Top module for ChipVerify project
  */
case class ChipVerify() extends AXKU041 {

  // CXP host blackbox
  val cxpHost = CxpHostWrapper()
  // CXP host <-> board connection
  cxpHost.UART <> UART
  led_test(1)  := False
  led_test(0)  := cxpHost.power_good

  // temp name for CXP <-> FMC-HPC card(part of FMC3)
  val fmc_ref_clk_n, fmc_ref_clk_p = in Bool ()
  val fmc_rx_n, fmc_rx_p           = in Bits (4 bits)
  val fmc_tx                       = out Bits (4 bits)
  val pocxp_en_tri_o               = out Bits (4 bits)

  cxpHost.fmc_ref_clk_n := fmc_ref_clk_n
  cxpHost.fmc_ref_clk_p := fmc_ref_clk_p
  cxpHost.fmc_rx_n      := fmc_rx_n
  cxpHost.fmc_rx_p      := fmc_rx_p

  fmc_tx         := cxpHost.fmc_tx
  pocxp_en_tri_o := cxpHost.pocxp_en_tri_o

  val dmaClockDomainConfig = ClockDomainConfig(resetActiveLevel = LOW)
  val dmaClockDomain =
    ClockDomain(cxpHost.dma_clk, True, config = dmaClockDomainConfig, frequency = FixedFrequency(250 MHz))

//  val dmaClockDomainConfig = ClockDomainConfig(clockEdge = RISING, resetKind = BOOT)
//  val dmaClockDomain = ClockDomain(cxpHost.dma_clk, config = dmaClockDomainConfig, frequency = FixedFrequency(250 MHz))

  // enable/set direction for inout signals
  useFmc1(asMaster = true)

  // module connection
  // AN9767 -> FL1010 -> FMC2
  val an9767 = AN9767()
  FL1010(
    an9767.alinx40PinOut,
    an9767.alinx40PinOut
  ) <> FMC1_LPC // copy an9767 output to both 40pin connectors on FL1010

  // PCIe module using Xillybus

  // for block design, RTL module cannot contains IP

  val devices = Seq(
    XillybusFifoRead("read_32", 32),
    XillybusFifoWrite("write_32", 32),
    XillybusFifoRead("read_8", 8),
    XillybusFifoWrite("write_8", 8),
    XillybusMemBi("mem_8", 8, 5)
  )
  val xillybus = XillybusWrapper(pinCount = 8, devices, this.device, Some(dmaClockDomain))
  xillybus.pcieXilinx <> pcie

  // stream loopback for PCIe test
  val upload_8   = xillybus.getStreamToHost("read_8")
  val download_8 = xillybus.getStreamFromHost("write_8")
  download_8 >> upload_8

  val upload_32   = xillybus.getStreamToHost("read_32")
  val download_32 = xillybus.getStreamFromHost("write_32")

  // creating system controller
  val ctrlArea =
    new ClockingArea(xillybus.pcieClockDomain) { // register file should be instantiated in the same clock domain as xillybus
      val memBusIf = XillybusBusIf(xillybus.memBus)
      // creating system controller using xillybus mem device
      val trigger = memBusIf // TODO: should be a WO register
        .newReg("trigger")
        .field(UInt(1 bits), AccessType.RW, 0, "trigger flag, generate pulse when this flag rise")
      val updatePulseGen = memBusIf // TODO: should be a WO register
        .newReg("updatePulseGen")
        .field(
          Bool(),
          AccessType.RW,
          0,
          "when true, host can write write_32 file to update the RAM inside pulse generator"
        )
      val triggerMode = memBusIf // TODO: should be a WO register
        .newReg("triggerMode")
        .field(UInt(1 bits), AccessType.RW, 0, "1 for single trigger, 0 for 1 + 10 trigger")
      val pulseWidthForSample = memBusIf
        .newReg("pulseWidthForSample")
        .field(UInt(8 bits), AccessType.RW, 0, "pulsewidth control word(PCW), pulsewidth = PCW * 0.5us")
      val voltageForSample = memBusIf
        .newReg("voltageForSample")
        .field(UInt(8 bits), AccessType.RW, 0, "voltage control word(VCW), voltage = VCW * 64 * (8.0V / 16383) - 4.0V")

      val pulseWidthForCamera = memBusIf
        .newReg("pulseWidthForCamera")
        .field(UInt(8 bits), AccessType.RW, 0, "pulse width in cycle")
      val voltageForCamera = memBusIf
        .newReg("voltageForCamera")
        .field(
          UInt(8 bits),
          AccessType.RW,
          0,
          "voltage control word(VCW), voltage = VCW * 64 * (8.0V / 16383) - 4.0V"
        )
      val delay0 = memBusIf
        .newReg("delay")
        .field(UInt(8 bits), AccessType.RW, 0, "camera pulse0 -> sample pulse delay, delay = CW * 0.5us")
      val delay1 = memBusIf
        .newReg("delay")
        .field(UInt(8 bits), AccessType.RW, 0, "sample pulse -> camera pulse1 delay, delay = CW * 0.5us")
      val delay2 = memBusIf
        .newReg("delay")
        .field(UInt(8 bits), AccessType.RW, 0, "camera pulse1 -> camera pulse2 -> ... delay, delay = CW * 0.5us")

      memBusIf.accept(PythonHeaderGenerator("regIf", "VerifyChip", "u32"))
      memBusIf.accept(HtmlGenerator("regIf", "VerifyChip"))

      // TODO: mark_debug conflict with async_reg
      // TODO: mark_debug on non-reg signal lead to unknown clock domain(which you need to specify manually)
      pulseWidthForSample.addAttribute("mark_debug", "true").setName("pulseWidth")
      delay0.addAttribute("mark_debug", "true").setName("delay")
      voltageForSample.addAttribute("mark_debug", "true").setName("voltage")
    }

  val dataArea =
    new ClockingArea(dmaClockDomain) {
      def getControlWordCC[T <: Data](controlWord: T) = {
        controlWord.addTag(crossClockDomain)
        controlWord.d(3)
      }
      val pulseGen = PulseGen(dmaClockDomain.frequency)
      // a trigger can be generated by host program through PCIe, or by pressing user key
      // it is the beginning of a series of events
      val trigger: Bool = getControlWordCC(ctrlArea.trigger).asBool.rise(False) || user_key.d(3).rise(False)
      pulseGen.trigger        := trigger
      pulseGen.updatePulseGen := getControlWordCC(ctrlArea.updatePulseGen)
      download_32             >> pulseGen.dataStreamIn
      // pulseGen -> DA(AN9767)
      Seq(an9767.channel1Clk, an9767.channel2Clk, an9767.channel1Wrt, an9767.channel2Wrt).foreach(
        _ := pulseGen.daClk
      ) // clk
      an9767.channel1 := pulseGen.pulseToSample
      an9767.channel2 := pulseGen.pulseToCamera

      // image processing
      val imageRx = ImageRx(44, 128)
      imageRx.clear  := trigger
      cxpHost.dmaOut <> imageRx.dmaIn

      imageRx.streamOut >> upload_32

      // debug
      trigger.addAttribute("mark_debug", "true").setName("trigger")
      pulseGen.pulseToCameraOn.addAttribute("mark_debug", "true").setName("pulseToCameraOn")
      pulseGen.pulseToSampleOn.addAttribute("mark_debug", "true").setName("pulseToSampleOn")
      imageRx.dmaIn.valid.addAttribute("mark_debug", "true").setName("imageValid")
      imageRx.dmaIn.ready.addAttribute("mark_debug", "true").setName("imageReady")
      imageRx.dmaIn.sol.addAttribute("mark_debug", "true").setName("imageSol")
      imageRx.dmaIn.eol.addAttribute("mark_debug", "true").setName("imageEol")
      imageRx.dmaIn.eop.addAttribute("mark_debug", "true").setName("imageEop")
      imageRx.dmaIn.empty.addAttribute("mark_debug", "true").setName("imageEmpty")

      out(SMA_CLKIN_P, SMA_CLKIN_N)
      SMA_CLKIN_P := imageRx.rxDone
      SMA_CLKIN_N := imageRx.rxDone

    }
}

object ChipVerify extends App {
  SpinalVerilog(ChipVerify())
}
