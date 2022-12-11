package Chainsaw.memory

import Chainsaw.deprecated.{ChainsawTest, DelayByRam}
import Chainsaw.{ChainsawFlatSpec, ChainsawImpl}
import spinal.core.sim.{SimConfig, _}

import scala.util.Random

class MemoryIpTests extends ChainsawFlatSpec {

  "belay by Ram" should "work" in {
    val width = 64
    val delay = 128
    val data = Seq.fill(1000)(BigInt(width, Random))
    ChainsawTest("testDelayByram", DelayByRam(width, delay), data).doTest()
  }

  it should "run at a high fmax with a huge size" in ChainsawImpl(DelayByRam(512, 1024), "implDelayByRam")

  val delayMax = 100
  val width = 32

  SimConfig.withFstWave.compile(DynamicDelay(width, delayMax)).doSim { dut =>
    import dut._

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
