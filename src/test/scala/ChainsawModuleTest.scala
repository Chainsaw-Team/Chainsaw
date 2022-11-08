import Chainsaw.ChainsawDuts
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._

class ChainsawModuleTest extends AnyFlatSpec {

  behavior of "auto validOut and lastOut"

  it should "work correctly" in { // TODO: more complete test

    val gen = ChainsawDuts.simpleDut(true)

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
