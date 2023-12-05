package Chainsaw.project.chipEval

import Chainsaw.DataUtil
import Chainsaw.edaFlow.PythonHeaderGenerator
import Chainsaw.edaFlow.boards.alinx.{AN9767, AXKU041, FL1010}
import Chainsaw.edaFlow.vivado.VivadoTask
import Chainsaw.xillybus._
import spinal.core.ClockDomain.ClockFrequency
import spinal.core._
import spinal.core.sim.SpinalSimConfig
import spinal.lib._
import spinal.lib.bus.regif.{AccessType, HtmlGenerator}

import scala.language.postfixOps

case class PulseGen(frequency:ClockFrequency) extends Module{

  val trigger = in Bool()
  val voltage0, voltage1, pulseWidth0, pulseWidth1, delay, period = in UInt(8 bits)
  val channel1, channel2 = out UInt(14 bits)
  val daClk = out(RegInit(False).toggleWhen(True)) // 100MHz DA clock

  val voltage0Full = voltage0 << 6
  val voltage1Full = voltage1 << 6

  // 0.5us counter
  val clkPeriod = 1.0 / frequency.getValue.toInt
  val stepCycle = (0.5e-6 / clkPeriod).toInt
  val stepCounter = CounterFreeRun(stepCycle)
  when(trigger)(stepCounter.clear())
  println(s"clk period = ${clkPeriod}, stepCycle = $stepCycle")
  // generate a pulse lasting 5us through channel 1 # FIXME: only generated when trigger 0 -> 1
  val pulseCounter = Counter(1 << 8)
  val pulseRun = RegInit(False)
  when(trigger)(pulseCounter.clear())
  when(trigger)(pulseRun.set())
  when(pulseRun & stepCounter.willOverflow)(pulseCounter.increment())
  when(pulseCounter === pulseWidth0)(pulseRun.clear())
  when(pulseRun)(channel1 := voltage0Full).otherwise(channel1 := 0)
  // generate 10 triggers for camera after certain delay
  val triggerCounter = Counter(1 << 8)
  val periodCounter = Counter(10)
  val triggerRun = RegInit(False)
  when(trigger)(triggerCounter.clear())
  when(trigger)(periodCounter.clear())
  when(trigger)(triggerRun.set())
  when(triggerRun & stepCounter.willOverflow)(triggerCounter.increment())
  when(triggerCounter === period)(triggerCounter.clear())
  when(triggerRun & triggerCounter === period)(periodCounter.increment())
  when(periodCounter.willOverflow)(triggerRun.clear())
  val triggerOn = (triggerCounter.value < pulseWidth1 + delay) & (triggerCounter.value >= delay) & triggerRun
  when(triggerOn)(channel2 := voltage1Full).otherwise(channel2 := 0)
}

object PulseGen extends App {
  SpinalSimConfig().withFstWave.compile(PulseGen(FixedFrequency(200 MHz))).doSim { dut =>
    import dut._
    import spinal.core.sim._

    clockDomain.forkStimulus(2)
    def triggerOnce() = {
      trigger #= false
      voltage0 #= 0
      voltage1 #= 0
      pulseWidth0 #= 0
      pulseWidth1 #= 0
      delay #= 0
      clockDomain.waitSampling()
      trigger #= true
      voltage0 #= 127
      voltage1 #= 63
      pulseWidth0 #= 10
      pulseWidth1 #= 10
      delay #= 5
      period #= 20
      clockDomain.waitSampling()
      trigger #= false
      clockDomain.waitSampling(50000)
    }
    triggerOnce()
    triggerOnce()

  }
}

case class ChipVerifyKu() extends AXKU041 {

  defaultClockDomain on {

    // enable/set direction for inout signals
    //    useFmc2(asMaster = true, dataOnly = true)
    useFmc1(asMaster = true)

    // module connection
    // AN9767 -> FL1010 -> FMC2
    val an9767 = AN9767()
    FL1010(an9767.alinx40PinOut, an9767.alinx40PinOut) <> FMC1_LPC

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
        xillybus.xillyDomain,
        xillybus.xillyDomain
      )
    )
    fifo_32.setName("fifo_32")
    fifo_8.setName("fifo_8")

    download_32    >> fifo_32.io.push
    fifo_32.io.pop >> upload_32
    download_8     >> fifo_8.io.push
    fifo_8.io.pop  >> upload_8

    // creating system controller
    val ctrlDomain = new ClockingArea(xillybus.xillyDomain) { // register file should be instantiated in the same clock domain as xillybus
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
        .field(UInt(8 bits), AccessType.RW, 0, "voltage control word(VCW), voltage = VCW * 64 * (8.0V / 16383) - 4.0V")
      val delayForCamera = memBusIf
        .newReg("delay")
        .field(UInt(8 bits), AccessType.RW, 0, "channel1 -> channel2 delay control word(DCW), delay = DCW * 0.5us")
      val pulseWidthForCamera = memBusIf
        .newReg("pulseWidthForCamera")
        .field(UInt(8 bits), AccessType.RW, 0, "pulse width in cycle")
      val voltageForCamera = memBusIf
        .newReg("voltageForCamera")
        .field(UInt(8 bits), AccessType.RW, 0, "voltage control word(VCW), voltage = VCW * 64 * (8.0V / 16383) - 4.0V")
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
    pulseGen.trigger := getControlWordCC(ctrlDomain.trigger).asBool.rise(False)
    pulseGen.trigger.addAttribute("mark_debug", "true")
    pulseGen.voltage0 := getControlWordCC(ctrlDomain.voltageForSample)
    pulseGen.voltage1 := getControlWordCC(ctrlDomain.voltageForCamera)
    pulseGen.pulseWidth0 := getControlWordCC(ctrlDomain.pulseWidthForSample)
    pulseGen.pulseWidth1 := getControlWordCC(ctrlDomain.pulseWidthForCamera)
    pulseGen.delay := getControlWordCC(ctrlDomain.delayForCamera)
    pulseGen.period := getControlWordCC(ctrlDomain.periodForCamera)

    Seq(an9767.channel1Clk, an9767.channel2Clk, an9767.channel1Wrt, an9767.channel2Wrt).foreach(_ := pulseGen.daClk) // clk
    an9767.channel1 := pulseGen.channel1
    an9767.channel2 := pulseGen.channel2

    // CXP subsystem

    // frame processing

    // ADC control

    // ILA showing delay

  }

}

object ChipVerifyKu extends App {
  SpinalVerilog(ChipVerifyKu())
//  VivadoTask.genBitStream(ChipVerifyKu(), "ChipVerifyKu")
}
