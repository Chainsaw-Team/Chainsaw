package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx.VivadoUtil
import cc.redberry.rings.scaladsl._
import spinal.core._

import scala.util.Random

/** implement big constant multiplication by compressor tree
  *
  * @param theConstant
  *   constant multiplicand
  * @param multiplierType
  *   MSB/LSB/FULL
  * @param widthIn
  *   width of variable multiplicand
  * @param widthInvolved
  *   width of bits involved in calculation
  * @param widthOut
  *   width of product
  * @param useCsd
  *   use canonical signed digit to represent the constant
  */
class BcmAlgo(
    theConstant: BigInt,
    override val multiplierType: MultiplierType,
    widthIn: Int,
    val widthInvolved: Int,
    override val widthOut: Int,
    useCsd: Boolean = false
) extends HardAlgo
    with MultAttribute {

  override def constant: Option[BigInt] = Some(theConstant)

  override def widthX = widthIn

  override def widthY = theConstant.bitLength

  override def dspCost = 0

  /** -------- width calculation
    * --------
    */
  val widthFull = theConstant.bitLength + widthIn

  // requirements
  multiplierType match {
    case FullMultiplier =>
      require(widthInvolved == widthOut && widthInvolved == widthFull)
    case MsbMultiplier => require(widthInvolved >= widthOut)
    case LsbMultiplier => require(widthInvolved == widthOut)
  }

  val targetSlice = multiplierType match { // range involved in calculation
    case FullMultiplier => 0 until widthFull
    case MsbMultiplier  => widthFull - widthInvolved until widthFull
    case LsbMultiplier  => 0 until widthInvolved
  }
  val widthNotInvolved =
    targetSlice.low // width dropped in calculation in MSB mode
  val widthNotOutputted =
    widthNotInvolved + (widthInvolved - widthOut) // width dropped in output in MSB mode

  /** -------- operands construction
    * --------
    */
  // get digits of the constant, low to high
  val constantDigits: String = (if (useCsd) Csd(theConstant).csd
                                else
                                  theConstant.toString(
                                    2
                                  )).reverse // low to high

  val sliceAndInfos: Seq[(IndexedSeq[Int], ArithInfo)] =
    constantDigits.zipWithIndex // digit and its weight
      .filterNot(_._1 == '0')   // skip 0s
      .map { case (c, weight) =>
        val range = weight until weight + widthIn
        val inter =
          range intersect targetSlice // get slices by intersection, this may be empty
        val dataSlice = inter.map(_ - weight) // inclusive slice
        (dataSlice, ArithInfo(dataSlice.length, weight, c == '1'))
      }
      .filterNot(_._1.isEmpty) // skip empty slices
      .map { case (slice, info) => (slice, info << slice.head) } // true weight

  val slices = sliceAndInfos.map(_._1)
  val infos  = sliceAndInfos.map(_._2)

  override def vivadoUtilEstimation =
    VivadoUtil(lut = sliceAndInfos.map(_._2).map(_.width).sum)

  // TODO: take compressor efficiency into consideration
  // TODO: more accurate estimation

  /** -------- calculation & verification
    * --------
    */

  def target(x: BigInt): BigInt = { // target of calculation
    require(x.bitLength <= widthIn)
    multiplierType match {
      case FullMultiplier => x * theConstant
      case MsbMultiplier =>
        (x * theConstant).toBitValue(widthFull).takeHigh(widthOut)
      case LsbMultiplier =>
        (x * theConstant).toBitValue(widthFull).takeLow(widthOut)
    }
  }

  def impl(x: BigInt): BigInt = { // actual calculation
    val raw = sliceAndInfos.map { case (slice, info) =>
      val segment = x.toBitValue()(slice.head to slice.last)
      val abs     = segment << info.weight
      if (info.isPositive) abs else -abs
    }.sum

    val ret = multiplierType match {
      case FullMultiplier => raw
      case MsbMultiplier  => raw >> widthNotOutputted
      case LsbMultiplier  => Zp(pow2(widthInvolved))(raw).toBigInt
    }

    multiplierType match { // in-place verification
      case FullMultiplier => assert(target(x) == ret)
      case MsbMultiplier =>
        assert(
          target(x) - ret <= upperBound && target(x) - ret >= lowerBound,
          s"error ${target(x) - ret}, upper $upperBound, lower $lowerBound"
        )
      case LsbMultiplier =>
        assert(
          Zp(pow2(widthInvolved))(ret).toBigInt == target(x),
          s"yours $ret, target ${target(x)}"
        )
    }

    ret
  }

  def selfTest(): Unit = {
    val data = Seq.fill(1000)(BigInt(widthIn, Random))
    data.foreach(impl)
    logger.info("truncated multiplication test passed")
  }

  def error(x: BigInt): BigInt = target(x) - impl(x)

  /** -------- error bound analysis for MSB mode
    * --------
    */
  val widthCoeff                                         = constantDigits.length
  var upperBound, lowerBound, dataForUpper, dataForLower = BigInt(0)
  if (multiplierType == MsbMultiplier) {
    (0 until widthIn).foreach {
      i => // accumulate the error bit by bit(low to high), as they are independent
        val widthDropped = (widthNotInvolved - i) min widthCoeff max 0

        val constantDropped = // the constant which a bit should have been multiplied by (its weight)
          if (useCsd) Csd(constantDigits.reverse).takeLow(widthDropped).evaluate
          else theConstant.toBitValue().takeLow(widthDropped)

        if (constantDropped >= BigInt(0)) {
          upperBound += constantDropped << i
          dataForUpper += BigInt(1)     << i
        } else {
          lowerBound += constantDropped << i
          dataForLower += BigInt(1)     << i
        }
    }

    upperBound = (upperBound >> widthNotOutputted) + 1
    lowerBound = lowerBound  >> widthNotOutputted

    assert(
      error(dataForUpper) <= upperBound && error(
        dataForUpper
      ) >= upperBound - 1,
      s"${error(dataForUpper)}, $upperBound"
    )
    assert(
      error(dataForLower) >= lowerBound && error(
        dataForLower
      ) <= lowerBound + 1,
      s"${error(dataForLower)}, $lowerBound"
    )
  }
}

object BcmAlgo {
  def apply(
      constant: BigInt,
      multiplierType: MultiplierType,
      widthIn: Int,
      widthInvolved: Int,
      widthOut: Int,
      useCsd: Boolean = false
  ): BcmAlgo = {
    new BcmAlgo(
      constant,
      multiplierType,
      widthIn,
      widthInvolved,
      widthOut,
      useCsd
    )
  }
}

object MsbConstantMult {
  def apply(
      constant: BigInt,
      widthIn: Int,
      widthInvolved: Int,
      widthOut: Int,
      useCsd: Boolean
  ) =
    BcmAlgo(constant, MsbMultiplier, widthIn, widthInvolved, widthOut, useCsd)
}

object LsbConstantMult {
  def apply(constant: BigInt, widthIn: Int, widthOut: Int, useCsd: Boolean) =
    BcmAlgo(constant, LsbMultiplier, widthIn, widthOut, widthOut, useCsd)
}

object FullConstantMult {
  def apply(constant: BigInt, widthIn: Int, useCsd: Boolean) =
    BcmAlgo(
      constant,
      FullMultiplier,
      widthIn,
      widthIn + constant.bitLength,
      widthIn + constant.bitLength,
      useCsd
    )
}
