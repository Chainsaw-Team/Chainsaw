package Chainsaw.arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw._
import Chainsaw.crypto.BarrettFineAlgo
import Chainsaw.xilinx._

import scala.util.Random

class BcmTest extends AnyFlatSpec {

  behavior of "toy system"

  val constantCount = 1

  def testToyBcm(multType: MultiplierType, useCsd: Boolean): Unit = {
    val widthIn = 16
    val constants = Seq.fill(constantCount)(BigInt(widthIn, Random))
    constants.foreach { constant =>
      val gen = multType match {
        case FullMultiplier => Bcm(constant, FullMultiplier, widthIn, widthIn + constant.bitLength, widthIn + constant.bitLength, useCsd)
        case MsbMultiplier => Bcm(constant, MsbMultiplier, widthIn, widthIn + 2, widthIn, useCsd)
        case LsbMultiplier => Bcm(constant, LsbMultiplier, widthIn, widthIn, widthIn, useCsd)
      }
      val data = Seq.fill(1000)(BigInt(widthIn, Random))
      ChainsawTest("testBcm", gen, data).doTest()
    }
  }

  it should "work for full multiplication, no csd" in testToyBcm(FullMultiplier, useCsd = false)

  it should "work for lsb multiplication, no csd" in testToyBcm(LsbMultiplier, useCsd = false)

  it should "work for msb multiplication, no csd" in testToyBcm(MsbMultiplier, useCsd = false)

  it should "work for full multiplication, csd" in testToyBcm(FullMultiplier, useCsd = true)

  it should "work for lsb multiplication, csd" in testToyBcm(LsbMultiplier, useCsd = true)

  it should "work for msb multiplication, csd" in testToyBcm(MsbMultiplier, useCsd = true)
}
