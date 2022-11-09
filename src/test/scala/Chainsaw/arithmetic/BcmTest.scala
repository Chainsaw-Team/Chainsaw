package Chainsaw.arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw._
import Chainsaw.xilinx._

import scala.util.Random

class BcmTest extends AnyFlatSpec {

  behavior of "toy system"

  def testBcm(width: Int, constant: BigInt, multType: MultiplierType, useCsd: Boolean): ChainsawTestReport = {
    val gen = Bcm(constant, width, multType, width, useCsd)
    val data = Seq.fill(5000)(BigInt(width, Random))
    val test = ChainsawTest("testBcm", gen, data)
    test.doTest()
  }

  it should "work for full multiplication, no csd" in {
    val constants = Seq.fill(50)(BigInt(16, Random))
    constants.foreach { constant =>
      logger.info(s"constant = $constant")
      testBcm(16, constant, FullMultiplier, useCsd = false)
    }
  }

  it should "work for lsb multiplication, no csd" in {
    val constants = Seq.fill(50)(BigInt(16, Random))
    constants.foreach { constant =>
      logger.info(s"constant = $constant")
      testBcm(16, constant, LsbMultiplier, useCsd = false)
    }
  }

  it should "work for msb multiplication, no csd" in {
    val constants = Seq.fill(50)(BigInt(16, Random))
    constants.foreach { constant =>
      logger.info(s"constant = $constant")
      testBcm(16, constant, MsbMultiplier, useCsd = false)
    }
  }

  it should "work for full multiplication, csd" in {
    val constants = Seq.fill(50)(BigInt(16, Random))
    constants.foreach { constant =>
      logger.info(s"constant = $constant")
      testBcm(16, constant, FullMultiplier, useCsd = true)
    }
  }

  it should "work for lsb multiplication, csd" in {
    val constants = Seq.fill(50)(BigInt(16, Random))
    constants.foreach { constant =>
      logger.info(s"constant = $constant")
      testBcm(16, constant, LsbMultiplier, useCsd = true)
    }
  }

  it should "work for msb multiplication, csd" in {
    val constants = Seq.fill(50)(BigInt(16, Random))
    constants.foreach { constant =>
      logger.info(s"constant = $constant")

      testBcm(16, constant, MsbMultiplier, useCsd = true)
    }
  }

  behavior of "systems for ZPRIZE"

  import project.zprize.ZPrizeMSM._

  val dataWidth = 377
  val data = Seq.fill(1000)(BigInt(dataWidth, Random))

  def msbMultGen = Bcm(MPrime, widthIn = dataWidth + 1, MsbMultiplier, dataWidth + 4, useCsd = true)
  def lsbMultGen = Bcm(baseModulus, widthIn = dataWidth, LsbMultiplier, dataWidth, useCsd = true)

  it should "work for ZPRIZE" in { // FIXME: sim won't run while synth will
    val msbTest = ChainsawTest("test377Msb", msbMultGen, data)
    val lsbTest = ChainsawTest("test377Lsb", lsbMultGen, data)
    msbTest.doTest()
    lsbTest.doTest()
  }

  // this result can be used in ISCA paper
  it should "show the cost of all configurations" in {
    VivadoSynth(msbMultGen.implH, "synth377Msb")
    VivadoSynth(lsbMultGen.implH, "synth377Lsb")
  }
}
