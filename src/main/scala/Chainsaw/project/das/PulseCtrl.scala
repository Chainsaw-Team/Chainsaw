package Chainsaw.project.das

import Chainsaw._
import Chainsaw.memory._
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._
import spinal.lib.bus.regif.{AccessType, BusIf}

import scala.language.postfixOps
case class PulseCtrl(busIf: BusIf, busClockDomain: ClockDomain, pulseBundle: PulseBundle, gain: UInt) extends Area {

  val busArea = new ClockingArea(busClockDomain) {
    // initialize cross clock domain registers, all "points" means number of clock cycles
    val pulse_period_points =
      busIf.newReg("pulse period control").field(word, AccessType.RW, 125000 / 2, "default 2kHz")
    val pulse_width_points =
      busIf.newReg("pulse width control").field(word, AccessType.RW, 13, "default 104ns")
    val pulse_delay_ctrl   = busIf.newReg("pulse delay control")
    val pulse_delay_points = pulse_delay_ctrl.field(byte, AccessType.RW, 0, "pulse1 -> pulse2 delay control")
    val pulse_fix_points   = pulse_delay_ctrl.field(byte, AccessType.RW, 0, "pulse1 -> frame header delay control")

    val gain_value = busIf
      .newReg("amplifier gain control")
      .field(UInt(6 bits), AccessType.RW, 37, "static gain, [28,63] -> [-9dB, +26dB]")
    val agc_points = busIf
      .newReg("automatic gain control")
      .field(word, AccessType.RW, 125000 / 2, "dynamic gain step, same as pulse points by default")
  }

  // pre-connection
  pulseBundle.sma0n.clear()
  pulseBundle.sma1n.clear()

  // pulse generation
  def getDynamicDelayed(signal: Bool, delay: UInt): Bool = {
    val delayModule = DynamicDelayModule(255, 1)
    delayModule.payloadIn := signal.asBits
    delayModule.delay     := delay
    delayModule.payloadOut.asBool
  }

  // FIXME: using pipelined comparator to generate pulseOn(or, glitch may appear)
  private val pulseCounter   = DynamicCounterFreeRun(getControlData(busArea.pulse_period_points))
  private val pulseOn        = (pulseCounter.value < getControlData(busArea.pulse_width_points)).d()
  private val pulseOnDelayed = getDynamicDelayed(pulseOn, getControlData(busArea.pulse_delay_points))
  private val pulseBack      = getDynamicDelayed(pulseOn, getControlData(busArea.pulse_fix_points))
  val pulseRise              = pulseBack.rise(False) // indicate that the Rayleigh scattering comes

  pulseBundle.sma0p := pulseOn
  pulseBundle.sma1p := pulseOnDelayed

  // automatic gain control
  private val gainCounter      = DynamicCounterFreeRun(getControlData(busArea.agc_points))
  private val gainValueCounter = Counter(64, inc = gainCounter.willOverflow)
  // sync with pulseOn
  when(pulseCounter.willOverflow) {
    gainCounter.clear()
    gainValueCounter.clear()
  }
  private val dynamicGain =
    (getControlData(busArea.gain_value) +| gainValueCounter.value)
      .d() // full gain = static gain + distance compensation, saturate at 63
  gain := U(63) - dynamicGain

  // debug
  pulseRise.simPublic()
}
