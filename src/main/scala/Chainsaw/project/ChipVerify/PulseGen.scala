package Chainsaw.project.ChipVerify

import spinal.core.ClockDomain.ClockFrequency
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps

/** Generating pulses for sample and camera
  */
case class PulseGen(frequency: ClockFrequency) extends Module {

  val trigger                                                     = in Bool ()
  val voltage0, voltage1, pulseWidth0, pulseWidth1, delay, period = in UInt (8 bits)
  val channel1, channel2                                          = out UInt (14 bits)
  val daClk = out(RegInit(False).toggleWhen(True)) // 100MHz DA clock

  val voltage0Full = voltage0 << 6
  val voltage1Full = voltage1 << 6
  val voltageZero  = U(0x1fff, 14 bits)

  // 0.5us counter
  val clkPeriod   = 1.0 / frequency.getValue.toInt
  val stepCycle   = (0.5e-6 / clkPeriod).toInt
  val stepCounter = CounterFreeRun(stepCycle)
  when(trigger)(stepCounter.clear())
  println(s"clk period = ${clkPeriod}, stepCycle = $stepCycle")
  // generate a pulse lasting 5us through channel 1 # FIXME: only generated when trigger 0 -> 1
  val pulseCounter = Counter(1 << 8)
  val pulseRun     = RegInit(False)
  when(trigger) { // start
    pulseCounter.clear()
    pulseRun.set()
  }
  when(pulseRun & stepCounter.willOverflow)(pulseCounter.increment()) // increment
  when(pulseCounter === pulseWidth0) {                                // end
//    pulseCounter.clear()
    pulseRun.clear()
  }
  when(pulseRun)(channel1 := voltage0Full).otherwise(channel1 := voltageZero) // state
  // generate 10 triggers for camera after certain delay
  val triggerCounter = Counter(1 << 8)
  val periodCounter  = Counter(10)
  val triggerRun     = RegInit(False)
  when(trigger) { // start
//    triggerCounter.clear()
//    periodCounter.clear()
    triggerRun.set()
  }
  // increment
  when(triggerRun & stepCounter.willOverflow)(triggerCounter.increment())
  when(triggerCounter === period)(triggerCounter.clear())
  when(triggerRun & triggerCounter === period)(periodCounter.increment())
  when(periodCounter.willOverflow) { // end
    triggerCounter.clear()
    periodCounter.clear()
    triggerRun.clear()
  }
  val triggerOn = (triggerCounter.value < pulseWidth1 + delay) & (triggerCounter.value >= delay) & triggerRun // state
  when(triggerOn)(channel2 := voltage1Full).otherwise(channel2 := voltageZero)
}

object PulseGen extends App {
  SpinalSimConfig().withFstWave.compile(PulseGen(FixedFrequency(200 MHz))).doSim { dut =>
    import dut._
    clockDomain.forkStimulus(2)
    def triggerOnce() = {
      trigger     #= false
      voltage0    #= 0
      voltage1    #= 0
      pulseWidth0 #= 0
      pulseWidth1 #= 0
      delay       #= 0
      clockDomain.waitSampling()
      trigger     #= true
      voltage0    #= 127
      voltage1    #= 63
      pulseWidth0 #= 10
      pulseWidth1 #= 10
      delay       #= 5
      period      #= 20
      clockDomain.waitSampling()
      trigger #= false
      clockDomain.waitSampling(50000)
    }
    triggerOnce()
    triggerOnce()

  }
}
