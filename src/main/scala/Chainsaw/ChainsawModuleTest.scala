package Chainsaw

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

class ChainsawModuleTest extends AnyFlatSpec {

  behavior of "auto validOut and lastOut"

  it should "work correctly" in { // TODO: more complete test

    val formatI = MatrixFormat(4, 10)

    val basic = Seq(-1, 0, 1, 2, 3, 4, 5, -1)
    val frame = FrameFormat(basic, 4)
    val formatO = frame.repeat(2).interpolate(2).pad(2)

    println(s"basic = $basic")
    println(s"formatO = $formatO")

    val gen: ChainsawGenerator = new ChainsawGenerator {
      override def name = "test"

      override def impl(dataIn: Seq[Any]) = dataIn

      override var inputTypes = Seq.fill(4)(UIntInfo(4))
      override var outputTypes = Seq.fill(4)(UIntInfo(4))

      override var inputFormat = formatI
      override var outputFormat = formatO
      override var latency = 5

      override def implH = new ChainsawModule(this) {
        dataOut := dataIn.d(latency)
      }
    }

    SimConfig.withFstWave.compile(gen.implH).doSim { dut =>
      dut.lastIn #= false
      dut.validIn #= false
      dut.clockDomain.forkStimulus(2)

      def pokeGoodFrame(): Unit = {
        (0 until gen.period - 1).foreach { _ =>
          dut.clockDomain.waitSampling()
          dut.lastIn #= false
          dut.validIn #= true
        }
        dut.clockDomain.waitSampling()
        dut.lastIn #= true
        dut.validIn #= true
      }

      // good continuous input
      (0 until 10).foreach(_ => pokeGoodFrame())

      // good input with breaks between frames
      (0 until 10).foreach { _ =>
        pokeGoodFrame()
        dut.clockDomain.waitSampling()
        dut.lastIn #= false
        dut.validIn #= false
      }

      // bad input where a frame is not continuous
      dut.clockDomain.waitSampling()
      dut.lastIn #= false
      dut.validIn #= true
      dut.clockDomain.waitSampling()
      dut.lastIn #= false
      dut.validIn #= false

      // bad input where an expected last didn't appear
      (0 until gen.period - 1).foreach { _ =>
        dut.clockDomain.waitSampling()
        dut.lastIn #= false
        dut.validIn #= true
      }
      dut.clockDomain.waitSampling()
      dut.lastIn #= false
      dut.validIn #= true
      dut.clockDomain.waitSampling(5)
    }
  }
}
