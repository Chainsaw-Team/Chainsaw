package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

case class Bcm(
    theConstant: BigInt,
    override val multiplierType: MultiplierType,
    widthIn: Int,
    override val widthInvolved: Int,
    override val widthOut: Int
) extends BcmAlgo(
      theConstant,
      multiplierType,
      widthIn,
      widthInvolved,
      widthOut,
      true
    )
    with UnsignedMultiplier {

  override def name =
    s"${className(multiplierType)}_Bcm_${widthIn}_${widthInvolved}_${widthOut}_${hashName(theConstant)}"

  val outputModulus = pow2(widthOut)
  val csaGen        = Merge(infos)

  override def latency() = 1

  override def fmaxEstimation: HertzNumber = 600 MHz

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    val yourData   = yours.head.toBigInt()
    val goldenData = golden.head.toBigInt()
    val det = multiplierType match {
      case FullMultiplier => yourData == goldenData
      case MsbMultiplier =>
        logger.info(
          s"$lowerBound <= error = ${goldenData - yourData} <= $upperBound"
        )
        lowerBound <= (goldenData - yourData) && (goldenData - yourData) <= upperBound
      case LsbMultiplier =>
        yourData.mod(outputModulus) == goldenData.mod(outputModulus)
    }

    if (!det)
      logger.info(
        s"\n----bcm error report----" +
          s"\n\t constant = $constant" +
          s"\n\t yourSum = $yourData" +
          s"\n\t goldenSum = $goldenData"
      )
    det
  }

  override def testCases = {
    val extra = Seq(dataForUpper, dataForLower).map(data =>
      TestCase(Seq(BigDecimal(data)))
    )
    super.testCases ++ extra
  }

  override def implH = new ChainsawOperatorModule(this) {
    val data = dataIn.head.asUInt()
    val operands = sliceAndInfos
      .map(_._1) // slices of input UInt
      .map { slice => data(slice.last downto slice.head) }
    val rawOutput = csaGen.sum(operands)
    dataOut.head := (multiplierType match {
      case MsbMultiplier =>
        (rawOutput >> widthNotOutputted).resize(widthOut).toAFix
      case _ => rawOutput.resize(widthOut).toAFix
    })
  }
}

object FullBcm {
  def apply(constant: BigInt, widthIn: Int) =
    Bcm(
      constant,
      MsbMultiplier,
      widthIn,
      widthIn + constant.bitLength,
      widthIn + constant.bitLength
    )
}

object MsbBcm {
  def apply(constant: BigInt, widthIn: Int, widthInvolved: Int, widthOut: Int) =
    Bcm(constant, MsbMultiplier, widthIn, widthInvolved, widthOut)
}

object LsbBcm {
  def apply(constant: BigInt, widthIn: Int, widthOut: Int) =
    Bcm(constant, LsbMultiplier, widthIn, widthOut, widthOut)
}
