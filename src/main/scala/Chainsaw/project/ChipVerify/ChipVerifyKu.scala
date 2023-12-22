package Chainsaw.project.ChipVerify

import Chainsaw.{BIN, DataUtil}
import Chainsaw.edaFlow.{PythonHeaderGenerator, xilinxDefaultSpinalConfig}
import Chainsaw.edaFlow.boards.alinx.{AN9767, AXKU041, FL1010}
import Chainsaw.edaFlow.vivado.VivadoTask
import Chainsaw.xillybus._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.{AccessType, HtmlGenerator}

import java.io.File
import scala.language.postfixOps
case class ChipVerifyKu() extends AXKU041 {

  defaultClockDomain on {

    // enable/set direction for inout signals
    useFmc1(asMaster = true)

    // module connection
    // AN9767 -> FL1010 -> FMC2
    val an9767 = AN9767()
    FL1010(an9767.alinx40PinOut, an9767.alinx40PinOut) <> FMC1_LPC // copy an9767 output to both 40pin connectors on FL1010

    // PCIe module using Xillybus
    val xillybus = XillybusWrapper.defaultWrapper(pinCount = 8, this.device)
    xillybus.pcieXilinx <> pcie

    // stream loopback
    val upload_32   = xillybus.getStreamToHost("read_32")
    val download_32 = xillybus.getStreamFromHost("write_32")
    val upload_8    = xillybus.getStreamToHost("read_8")
    val download_8  = xillybus.getStreamFromHost("write_8")

    val Seq(fifo_32, fifo_8) = Seq(32, 8).map(bitWidth =>
      StreamFifoCC(
        dataType = Bits(bitWidth bits),
        depth    = 1024,
        xillybus.pcieClockDomain,
        xillybus.pcieClockDomain
      )
    )
    fifo_32.setName("fifo_32")
    fifo_8.setName("fifo_8")

    download_32    >> fifo_32.io.push
    fifo_32.io.pop >> upload_32
    download_8     >> fifo_8.io.push
    fifo_8.io.pop  >> upload_8

    // creating system controller
    val ctrlDomain =
      new ClockingArea(xillybus.pcieClockDomain) { // register file should be instantiated in the same clock domain as xillybus
        val memBusIf = XillybusBusIf(xillybus.memBus)
        // creating system controller using xillybus mem device
        val trigger = memBusIf
          .newReg("trigger")
          .field(UInt(1 bits), AccessType.RW, 0, "trigger flag, generate pulse when this flag change")
        val pulseWidthForSample = memBusIf
          .newReg("pulseWidthForSample")
          .field(UInt(8 bits), AccessType.RW, 0, "pulsewidth control word(PCW), pulsewidth = PCW * 0.5us")
        val voltageForSample = memBusIf
          .newReg("voltageForSample")
          .field(
            UInt(8 bits),
            AccessType.RW,
            0,
            "voltage control word(VCW), voltage = VCW * 64 * (8.0V / 16383) - 4.0V"
          )
        val delayForCamera = memBusIf
          .newReg("delay")
          .field(UInt(8 bits), AccessType.RW, 0, "channel1 -> channel2 delay control word(DCW), delay = DCW * 0.5us")
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
        val periodForCamera = memBusIf
          .newReg("periodForCamera")
          .field(UInt(8 bits), AccessType.RW, 0, "period = CW * 0.5us")

        memBusIf.accept(PythonHeaderGenerator("regIf", "VerifyChip", "u32"))
        memBusIf.accept(HtmlGenerator("regIf", "VerifyChip"))

        // TODO: mark_debug conflict with async_reg
        // TODO: mark_debug on non-reg signal lead to unknown clock domain(which you need to specify manually)
        pulseWidthForSample.addAttribute("mark_debug", "true").setName("pulseWidth")
        delayForCamera.addAttribute("mark_debug", "true").setName("delay")
        voltageForSample.addAttribute("mark_debug", "true").setName("voltage")
      }

    def getControlWordCC[T <: Data](controlWord: T) = {
      controlWord.addTag(crossClockDomain)
      controlWord.d(3)
    }

    val pulseGen = PulseGen(defaultClockDomain.frequency)
    // a trigger can be generated by host program through PCIe, or by pressing user key
    val trigger = getControlWordCC(ctrlDomain.trigger).asBool.rise(False) || user_key.d(3).rise(False)
    trigger.addAttribute("mark_debug", "true")
    pulseGen.trigger := trigger
    pulseGen.voltageForSample    := getControlWordCC(ctrlDomain.voltageForSample)
    pulseGen.voltage1ForCamera    := getControlWordCC(ctrlDomain.voltageForCamera)
    pulseGen.pulseWidthForSample := getControlWordCC(ctrlDomain.pulseWidthForSample)
    pulseGen.pulseWidthForCamera := getControlWordCC(ctrlDomain.pulseWidthForCamera)
    pulseGen.delay0       := getControlWordCC(ctrlDomain.delayForCamera)
    pulseGen.delay1      := getControlWordCC(ctrlDomain.periodForCamera)

    Seq(an9767.channel1Clk, an9767.channel2Clk, an9767.channel1Wrt, an9767.channel2Wrt).foreach(
      _ := pulseGen.daClk
    ) // clk
    an9767.channel1 := pulseGen.pulseToSample
    an9767.channel2 := pulseGen.pulseToCamera

    // CXP subsystem

    // frame processing

    // ADC control

    // ILA showing delay


  }

}

object ChipVerifyKu extends App {
  SpinalVerilog(ChipVerifyKu())
//  VivadoTask.genBitStreamForBoard("ChipVerifyKu", ChipVerifyKu())
}
