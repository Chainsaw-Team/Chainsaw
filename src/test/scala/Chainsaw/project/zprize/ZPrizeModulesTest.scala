package Chainsaw.project.zprize

import Chainsaw._
import Chainsaw.arithmetic.Bcm
import Chainsaw.arithmetic.Bm
import Chainsaw.crypto.BarrettFineAlgo
import Chainsaw.crypto.Barrett
import Chainsaw.project.zprize.ZPrizeModules._
import Chainsaw.project.zprize.ZPrizeMSM.{baseModulus, poseidonModulus}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ZPrizeModulesTest extends AnyFlatSpec {

  behavior of "BCMs"

  val dataWidth = 377
  val data = Seq.fill(1000)(BigInt(dataWidth, Random)) // compilation takes 2 minutes, 100000 takes 27 minutes, 1000 takes

  ignore should "work for ZPRIZE MSB mult" in ChainsawTest("test377Msb", msbMultGen, data).doTest()
  ignore should "work for ZPRIZE LSB mult" in ChainsawTest("test377Lsb", lsbMultGen, data).doTest()

  behavior of "Barrett"

  /** --------
   * poseidon
   * -------- */
  val poseidonGen = Barrett(255, Some(poseidonModulus), FullMultiplier)

  ignore should "work for poseidon modulus" in BarrettFineAlgo(poseidonModulus).selfTest()
  ignore should "work on hardware for poseidon modulus" in {
    setAsNaive(Bcm, Bm)
    poseidonGen.doSelfTest()
  }

  ignore should "synth for poseidon modulus" in ChainsawSynth(poseidonGen, "synthPoseidon")
  ignore should "impl for poseidon modulus" in ChainsawImpl(poseidonGen, "implPoseidon")

  /** --------
   * zprize
   * -------- */
  val zprizeGen = Barrett(377, Some(baseModulus), FullMultiplier)

  ignore should "work for zprize msm" in BarrettFineAlgo(baseModulus).selfTest()

  ignore should "work on hardware for zprize msm" in {
    setAsNaive(Bcm, Bm)
    zprizeGen.doSelfTest()
  }

  ignore should "impl for zprize msm" in ChainsawSynth(zprizeGen, "synthZPrize")

}