package Chainsaw.memory

import Chainsaw.deprecated.{ChainsawTest, DelayByRam}
import Chainsaw.xilinx.VivadoSynth
import Chainsaw.{ChainsawFlatSpec, ChainsawImplOld, TestConfig}
import spinal.core.sim.{SimConfig, _}

import scala.util.Random

class MemoryIpTests extends ChainsawFlatSpec {



  //  "belay by Ram" should "work" in {
  //    val width = 64
  //    val delay = 128
  //    val data = Seq.fill(1000)(BigInt(width, Random))
  //    ChainsawTest("testDelayByram", DelayByRam(width, delay), data).doTest()
  //  }
  //
  //  it should "run at a high fmax with a huge size" in ChainsawImplOld(DelayByRam(512, 1024), "implDelayByRam")


  def testP2S(): Unit = {
    testOperator(P2S(8, 4, 16), generatorConfigTable("P2S"))
  }
  //
  //
  //  val delayMax = 100
  //  val width = 32
  //
  //  SimConfig.withFstWave.compile(DynamicDelay(width, delayMax)).doSim { dut =>
  //    import dut._
  //
  //    def locked = lockedOut.toBoolean
  //
  //    def actuallyLocked(i: Int, delay: Int) = i - dataOut.toBigInt == delay
  //
  //    delayIn #= 13
  //    clockDomain.forkStimulus(2)
  //    clockDomain.waitSampling()
  //    (0 until 100).foreach { i =>
  //      dataIn #= i
  //      clockDomain.waitSampling()
  //      if (locked) assert(actuallyLocked(i, 13))
  //    }
  //
  //    delayIn #= 21
  //    clockDomain.waitSampling() // state transition
  //
  //    (0 until 200).foreach { i =>
  //      dataIn #= i
  //      clockDomain.waitSampling()
  //      if (locked) assert(actuallyLocked(i, 21))
  //    }
  //  }
  //
  //  behavior of "general ram"
  //
  //  it should "show the frequency of cascading BRAM/URAMs" in {
  //
  //    val cascadingNumber = Seq(1, 4, 16)
  //    val uramFreqs = cascadingNumber.map { n =>VivadoSynth(SingleRam(72, (1 << 12) * n, 10, 20), s"synthUramArray_$n")}
  //    val bramFreqs = cascadingNumber.map { n =>VivadoSynth(DoubleRam(72, (1 << 9) * n, 10, 20), s"synthBramArray_$n")}
  //    println(s"uram freqs: ${uramFreqs.mkString(" ")}")
  //    println(s"bram freqs: ${bramFreqs.mkString(" ")}")
  //  }

  override def generatorConfigTable = Map(
    "P2S" -> TestConfig(full = true, naive = false, synth = true, impl = true),
  )

  testP2S()
}
