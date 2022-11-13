package Chainsaw.arithmetic

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class TruncatedConstantMultTest extends AnyFlatSpec {

  val width = 377
  val constant = Chainsaw.project.zprize.ZPrizeMSM.baseModulus
  val data = Seq.fill(10000)(BigInt(377, Random))

  "full mode" should "work" in {
    val mult = FullConstantMult(constant, width, useCsd = true)
    data.foreach(mult.impl)
  }

  "msb mode" should "work" in {
    val msbMult0 = MsbConstantMult(constant, widthIn = width, widthInvolved = width, widthOut = width, useCsd = true)
    logger.info(s"error bound of msbMult0: ${msbMult0.lowerBound}, ${msbMult0.upperBound}")
    data.foreach(msbMult0.impl)

    val msbMult1 = MsbConstantMult(constant, widthIn = width, widthInvolved = width + 4, widthOut = width, useCsd = true)
    logger.info(s"error bound of msbMult1: ${msbMult1.lowerBound}, ${msbMult1.upperBound}")
    data.foreach(msbMult1.impl)

    val msbMult2 = MsbConstantMult(constant, widthIn = width, widthInvolved = width + 8, widthOut = width, useCsd = true)
    logger.info(s"error bound of msbMult2: ${msbMult2.lowerBound}, ${msbMult2.upperBound}")
    data.foreach(msbMult2.impl)
  }

  "lsb mode" should "work" in {
    val lsbMult = LsbConstantMult(constant, widthIn = width, widthOut = width, useCsd = true)
    data.foreach(lsbMult.impl)
  }


}
