package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

case class Bcm(theConstant: BigInt,
               override val multiplierType: MultiplierType,
               widthIn: Int,
               widthInvolved: Int,
               override val widthOut: Int)
  extends UnsignedMultiplier {

  def algo = BcmAlgo(theConstant, multiplierType, widthIn, widthInvolved, widthOut, useCsd = true)
  val outputModulus = Pow2(widthOut)
  override val widthX = widthIn
  override val widthY = theConstant.bitLength
  override val constant = Some(theConstant)

  override def latency() = 1

  override def name = s"${className(multiplierType)}_Bcm_${widthIn}_${widthInvolved}_${widthOut}_${hashName(theConstant)}"

  override def vivadoUtilEstimation = algo.vivadoUtilEstimation

  override def fmaxEstimation = 600 MHz

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    val yourData = yours.head.toBigInt()
    val goldenData = golden.head.toBigInt()
    val det = multiplierType match {
      case FullMultiplier => yourData == goldenData
      case MsbMultiplier => (yourData - goldenData).abs <= 1 // introduced by compensation shift
      case LsbMultiplier => yourData.mod(outputModulus) == goldenData.mod(outputModulus)
    }

    if (!det) logger.info(
      s"\n----bcm error report----" +
        s"\n\t constant = $constant" +
        s"\n\t yourSum = $yourData" +
        s"\n\t goldenSum = $goldenData"
    )
    det
  }

  override def implH = ???
}

object FullBcm {
  def apply(constant: BigInt, widthIn: Int) =
    Bcm(constant, MsbMultiplier, widthIn, widthIn + constant.bitLength, widthIn + constant.bitLength)
}

object MsbBcm {
  def apply(constant: BigInt, widthIn: Int, widthInvolved: Int, widthOut: Int) =
    Bcm(constant, MsbMultiplier, widthIn, widthInvolved, widthOut)
}

object LsbBcm {
  def apply(constant: BigInt, widthIn: Int, widthOut: Int) =
    Bcm(constant, LsbMultiplier, widthIn, widthOut, widthOut)
}
