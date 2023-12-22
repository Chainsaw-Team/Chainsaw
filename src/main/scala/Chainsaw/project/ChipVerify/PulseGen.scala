package Chainsaw.project.ChipVerify

import spinal.core.ClockDomain.ClockFrequency
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm.{State, StateDelay, StateMachine}

import scala.language.postfixOps

/** Generating pulses for sample and camera
  */
case class PulseGen(frequency: ClockFrequency) extends Module {

  // I/O
  val trigger     = in Bool ()
  val triggerMode = in UInt (1 bits)
  val voltageForSample, voltage1ForCamera, pulseWidthForSample, pulseWidthForCamera, delay0, delay1, delay2 =
    in UInt (8 bits)
  val pulseToSample, pulseToCamera     = out UInt (14 bits)
  val pulseToSampleOn, pulseToCameraOn = out Bool ()
  val daClk                            = out(RegInit(False).toggleWhen(True)) // 125MHz DA clock

  // params
  val voltageForSampleFull = voltageForSample  << 6
  val voltageForCameraFull = voltage1ForCamera << 6
  val voltageZero          = U(0x1fff, 14 bits)
  val cameraFrameCount     = 10 // frames after ref frame

  // 0.5us counter
  val clkPeriod   = 1.0 / frequency.getValue.toInt
  val stepCycle   = (0.5e-6 / clkPeriod).toInt
  val stepCounter = CounterFreeRun(stepCycle)
  println(s"clk period = $clkPeriod, stepCycle = $stepCycle")

  pulseToCameraOn := False
  pulseToSampleOn := False

  val stateCounter = Counter(1 << 8)
  val pulseCounter = Counter(cameraFrameCount)
  val pulseGenFsm = new StateMachine {
    val BOOT                                    = makeInstantEntry()
    val IDLE, CAM_PULSE0, SAM_PULSE, CAM_PULSE1 = new State

    // state transition logic
    BOOT.whenIsActive(goto(IDLE))
    IDLE.whenIsActive(when(trigger)(goto(CAM_PULSE0)))
    CAM_PULSE0.whenIsActive {
      when(stateCounter === delay0 - 1 & stepCounter.willOverflow) {
        when(triggerMode === U(1))(goto(IDLE)).otherwise(goto(SAM_PULSE))
      }
    }
    SAM_PULSE.whenIsActive(when(stateCounter === delay1 - 1 & stepCounter.willOverflow)(goto(CAM_PULSE1)))
    CAM_PULSE1.whenIsActive(when(pulseCounter.willOverflow)(goto(IDLE)))

    // state working logic
    when(stepCounter.willOverflow && !isActive(IDLE))(stateCounter.increment())

    CAM_PULSE0.onEntry {
      stepCounter.clear()
      stateCounter.clear()
    }
    CAM_PULSE0.whenIsActive(when(stateCounter.value < pulseWidthForCamera)(pulseToCameraOn.set()))
    SAM_PULSE.onEntry(stateCounter.clear())
    SAM_PULSE.whenIsActive(when(stateCounter.value < pulseWidthForSample)(pulseToSampleOn.set()))
    CAM_PULSE1.onEntry {
      stateCounter.clear()
      pulseCounter.clear()
    }
    CAM_PULSE1.whenIsActive {
      when(stateCounter.value < pulseWidthForCamera)(pulseToCameraOn.set())
      when(stateCounter === delay2 - 1 & stepCounter.willOverflow) {
        pulseCounter.increment()
        stateCounter.clear()
      }
    }

  }

  pulseToCamera := Mux(pulseToCameraOn, voltageForCameraFull, voltageZero)
  pulseToSample := Mux(pulseToSampleOn, voltageForSampleFull, voltageZero)
}

object PulseGen extends App {
  SpinalSimConfig().withFstWave.compile(PulseGen(FixedFrequency(200 MHz))).doSim { dut =>
    import dut._
    clockDomain.forkStimulus(2)

    def triggerOnce(mode: Int) = {
      trigger             #= false
      triggerMode         #= mode
      voltageForSample    #= 0
      voltage1ForCamera   #= 0
      pulseWidthForSample #= 0
      pulseWidthForCamera #= 0
      delay0              #= 0
      clockDomain.waitSampling()

      trigger             #= true
      voltageForSample    #= 255
      voltage1ForCamera   #= 255
      pulseWidthForSample #= 10
      pulseWidthForCamera #= 2
      delay0              #= 30
      delay1              #= 30
      delay2              #= 15
      clockDomain.waitSampling()
      trigger #= false
      clockDomain.waitSampling(25000)
    }
    triggerOnce(1)
    triggerOnce(0)
    triggerOnce(1)
    triggerOnce(0)

  }
}
