package Chainsaw.memory

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._

class DynamicDelayTest extends AnyFlatSpec {

  val delayMax = 100
  val width = 32

  SimConfig.withFstWave.compile(DynamicDelay(width, delayMax)).doSim { dut =>
    import dut.{clockDomain, dataIn, dataOut, delayIn, lockedOut}

    def locked = lockedOut.toBoolean

    def actuallyLocked(i: Int, delay: Int) = i - dataOut.toBigInt == delay

    delayIn #= 13
    clockDomain.forkStimulus(2)
    clockDomain.waitSampling()
    (0 until 100).foreach { i =>
      dataIn #= i
      clockDomain.waitSampling()
      if (locked) assert(actuallyLocked(i, 13))
    }

    delayIn #= 21
    clockDomain.waitSampling() // state transition

    (0 until 200).foreach { i =>
      dataIn #= i
      clockDomain.waitSampling()
      if (locked) assert(actuallyLocked(i, 21))
    }
  }

}
