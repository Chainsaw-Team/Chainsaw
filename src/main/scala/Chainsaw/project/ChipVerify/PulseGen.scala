package Chainsaw.project.ChipVerify

import Chainsaw.DataUtil
import spinal.core.ClockDomain.ClockFrequency
import spinal.core._
import spinal.core.sim._
import spinal.lib.{Stream, _}
import spinal.lib.fsm.{State, StateDelay, StateMachine}

import scala.language.postfixOps

/** Generating pulses for sample and camera
  */
case class PulseGen(frequency: ClockFrequency) extends Module {

  // I/O
  val updatePulseGen = in Bool ()
  val dataStreamIn   = slave Stream (Bits(32 bits))

  val trigger                      = in Bool ()
  val pulseToSample, pulseToCamera = out UInt (14 bits)
  val daClk                        = out(RegInit(False).toggleWhen(True)) // 125MHz DA clock

  // params
  val voltageZero = U(0x1fff, 14 bits)

  // 0.5us counter
  val clkPeriod = 1.0 / frequency.getValue.toInt
  val stepCycle = (0.5e-6 / clkPeriod).toInt
//  val stepCycle   = 10
  val stepCounter = CounterFreeRun(stepCycle)
  println(s"clk period = $clkPeriod, stepCycle = $stepCycle")

  // components
  val ramDepth      = 2000
  val allOnes       = Seq.fill(ramDepth)(B(0xffff, 16 bits))
  val cameraDataRam = Mem(allOnes)
  val sampleDataRam = Mem(allOnes)
  val ramCounter    = Counter(ramDepth)

  // pre-assignment
  pulseToCamera      := voltageZero
  pulseToSample      := voltageZero
  dataStreamIn.ready := False

  val pulseGenFsm = new StateMachine {
    val BOOT                    = makeInstantEntry()
    val IDLE, UPDATE, TRIGGERED = new State
    val CLEAR                   = new StateDelay(10000) // clear data in xillybus buffer

    // state transition logic
    BOOT.whenIsActive(goto(IDLE))
    IDLE.whenIsActive {
      when(trigger)(goto(TRIGGERED))
      when(updatePulseGen)(goto(CLEAR))
    }
    CLEAR.whenCompleted(goto(UPDATE))
    TRIGGERED.whenIsActive(when(ramCounter.willOverflow)(goto(IDLE)))
    UPDATE.whenIsActive { when(ramCounter.willOverflow)(goto(IDLE)) }

    // workload
    IDLE.onEntry {
      stepCounter.clear()
      ramCounter.clear()
    }
    TRIGGERED.onEntry {
      stepCounter.clear()
      ramCounter.clear()
    }
    TRIGGERED.whenIsActive {
      when(stepCounter.willOverflow)(ramCounter.increment())
    }
    when(isActive(TRIGGERED).d()) {
      pulseToCamera := cameraDataRam.readSync(ramCounter.value).takeLow(14).asUInt
      pulseToSample := sampleDataRam.readSync(ramCounter.value).takeLow(14).asUInt
    }
    CLEAR.whenIsActive(dataStreamIn.ready := True)
    UPDATE.onEntry {
      stepCounter.clear()
      ramCounter.clear()
    }
    UPDATE.whenIsActive {
      dataStreamIn.ready := True
      when(dataStreamIn.valid) {
        ramCounter.increment()
        cameraDataRam.write(ramCounter.value, dataStreamIn.payload(31 downto 16))
        sampleDataRam.write(ramCounter.value, dataStreamIn.payload(15 downto 0))
      }
    }
  }

  // debug
  val pulseToSampleOn, pulseToCameraOn = out Bool ()
  pulseToSampleOn := (pulseToSample =/= voltageZero).d()
  pulseToCameraOn := (pulseToCamera =/= voltageZero).d()
}

object PulseGen extends App {
  SpinalSimConfig().withFstWave.compile(PulseGen(FixedFrequency(200 MHz))).doSim { dut =>
    import dut._
    clockDomain.forkStimulus(2)

    val cameraSeq = (0 until 2000).map(BigInt(_))
    val sampleSeq = (0 until 2000).reverse.map(BigInt(_))

    def loopOnce() = {
      // init
      trigger              #= false
      updatePulseGen       #= false
      dataStreamIn.valid   #= false
      dataStreamIn.payload #= 0x12345678
      clockDomain.waitSampling()
      // start CLEAR by a pulse on updatePulseGen
      updatePulseGen #= true
      clockDomain.waitSampling()
      updatePulseGen #= false
      // wait for clear
      clockDomain.waitSampling(15000)
      // update ram
      (0 until dut.ramDepth).foreach { i =>
        dataStreamIn.valid   #= true
        dataStreamIn.payload #= (cameraSeq(i) << 16) + sampleSeq(i)
        clockDomain.waitSampling()
      }

      updatePulseGen #= false
      clockDomain.waitSampling()

      // trigger once
      // start TRIGGERED by a pulse on trigger
      trigger #= true
      clockDomain.waitSampling()
      trigger #= false
      // wait for pulse output
      clockDomain.waitSampling(stepCycle * ramDepth)

      trigger #= true
      clockDomain.waitSampling()
      trigger #= false
      clockDomain.waitSampling(stepCycle * ramDepth)
    }

    loopOnce()
  }
}
