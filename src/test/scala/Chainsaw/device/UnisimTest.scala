package Chainsaw.device

import Chainsaw._
import Chainsaw.xilinx._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

class UnisimTest extends AnyFlatSpec {

  // TODO: use ChainsawFlatSpec
  // TODO: primitives need more encapsulation like this
  case class Lut6Dut(init: BigInt) extends Component {
    val dataIn  = in Bits (6 bits)
    val dataOut = out Bits (2 bits)
    val lut6    = LUT6_2(init)
    lut6.I0    := dataIn(0)
    lut6.I1    := dataIn(1)
    lut6.I2    := dataIn(2)
    lut6.I3    := dataIn(3)
    lut6.I4    := dataIn(4)
    lut6.I5    := dataIn(5)
    dataOut(0) := lut6.O5
    dataOut(1) := lut6.O6
  }

  "LUT6" should "synth" in {
    if (hasVivado)
      VivadoSynth(Lut6Dut(BigInt(0)), "lut6")
        .requireUtil(VivadoUtil(lut = 1), PreciseRequirement)
  }

  it should "work" in SimConfig.withFstWave
    .compile {
      Lut6Dut(pow2(32) + 1)
    }
    .doSim { dut =>
      (0 until 64).foreach { i =>
        dut.dataIn #= BigInt(i)
        sleep(1)
        println(s"lookup $i ${dut.dataOut.toBigInt.toString(2)}")
      }
    }

  behavior of "CARRY8"

  // TODO: add DUT
  it should "synth" in {
    if (hasVivado)
      VivadoSynth(CARRY8(), "carry8")
        .requireUtil(VivadoUtil(carry8 = 1), PreciseRequirement)
  }

  // TODO: add assertions
  it should "work" in SimConfig.withFstWave.compile(CARRY8()).doSim { dut =>
    dut.S      #= 255
    dut.DI     #= 255
    dut.CI_TOP #= false
    dut.CI     #= true
    sleep(1)
  }
}
