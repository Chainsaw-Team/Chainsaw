package Chainsaw.arithmetic

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class TruncatedConstantMultTest extends AnyFlatSpec {

  behavior of "truncated constant multiplier"

  val widthIn = 377
  val constant = BigInt(376, Random) + BigInt(1) << 377

  val extraWidths = Seq(0, 4, 8)
  val multTypes = Seq(FullMultiplier, MsbMultiplier, LsbMultiplier)
  val useCsds = Seq(true, false)

  multTypes.foreach(multType =>
    useCsds.foreach { useCsd =>
      val extras = if (multType == MsbMultiplier) extraWidths else Seq(0)
      extras.foreach { extraWidth =>
        it should s"work for $widthIn bit ${className(multType)} using ${widthIn + extraWidth} bit and ${if (useCsd) "csd" else "binary"} encoding" in {
          val algo = multType match {
            case FullMultiplier => TruncatedConstantMult(constant, multType, widthIn, widthIn + constant.bitLength, widthIn + constant.bitLength, useCsd)
            case MsbMultiplier => TruncatedConstantMult(constant, multType, widthIn, widthIn + extraWidth, widthIn, useCsd)
            case LsbMultiplier => TruncatedConstantMult(constant, multType, widthIn, widthIn, widthIn, useCsd)
          }
          if (multType == MsbMultiplier) logger.info(s"error bound of msbMult0: ${algo.lowerBound}, ${algo.upperBound}")
          algo.selfTest()
        }
      }
    }
  )
}