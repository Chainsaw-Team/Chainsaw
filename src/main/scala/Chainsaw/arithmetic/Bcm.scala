package Chainsaw.arithmetic

import Chainsaw._
import cc.redberry.rings.scaladsl._
import spinal.core._

import scala.language.postfixOps

/** implement big constant multiplication by compressor tree
 *
 * @see [[Csd]]
 * @see [[TruncatedConstantMult]]
 */
case class Bcm(constant: BigInt, multiplierType: MultiplierType, widthIn: Int, widthInvolved: Int, widthOut: Int, useCsd: Boolean = false)
  extends ChainsawGenerator {

  val model = TruncatedConstantMult(constant, multiplierType, widthIn, widthInvolved, widthOut, useCsd)

  import model._

  override def name = s"bcm_${constant.hashCode()}_${targetSlice.start}_until_${targetSlice.end}".replace("-", "N")

  override var inputTypes = Seq(UIntInfo(widthIn))
  override var outputTypes = Seq.fill(2)(UIntInfo(widthOut))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  val compressorGen = BitHeapCompressor(sliceAndInfos.map(_._2))
  override var latency = compressorGen.latency

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val data = dataIn.asInstanceOf[Seq[BigInt]].head
    val ret = model.impl(data)
    Seq(ret, BigInt(0)) // to match the output size
  }

  val compensation = compressorGen.compensation >> model.widthNotOutputted

  val frameWiseMetric = (yours: Seq[Any], golden: Seq[Any]) => {

    val goldenSum = golden.asInstanceOf[Seq[BigInt]].sum

    val yourSum = yours.asInstanceOf[Seq[BigInt]].sum - compensation // can be implemented by a following CPA

    val det = multiplierType match {
      case FullMultiplier => yourSum == goldenSum
      case MsbMultiplier =>
        (yourSum - goldenSum).abs <= 1 // introduced by compensation shift
      case LsbMultiplier =>
        val modulus = Pow2(widthInvolved)
        Zp(modulus)(yourSum) == Zp(modulus)(goldenSum)
    }

    if (!det) logger.info(
      s"\n----bcm error report----" +
        s"\n\t constant = $constant" +
        s"\n\t yourSum = $yourSum" +
        s"\n\t goldenSum = $goldenSum"
    )
    det
  }

  override val metric = ChainsawMetric(frameWise = frameWiseMetric)

  if (multiplierType == MsbMultiplier) {
    logger.info(
      s"\n----error analysis for big constant multiplier at MSB mode----" +
        s"\n\terror bound of MSB multiplication: [$lowerBound, $upperBound]" +
        s"\n\tlower bound achieved by $dataForLower" +
        s"\n\tupper bound achieved by $dataForUpper")
  }

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val data = uintDataIn.head
    val operands = sliceAndInfos.map(_._1) // slices
      .map { slice => data(slice.last downto slice.head) }

    val core = compressorGen.getImplH
    core.dataIn := operands.map(_.asBits)

    val ret = multiplierType match {
      case MsbMultiplier => core.dataOut.map(_ >> widthNotOutputted).map(_.resize(widthOut))
      case _ => core.dataOut.map(_.resize(widthOut))
    }

    dataOut := ret
  }
}

object MsbBcm {
  def apply(constant: BigInt, widthIn: Int, widthInvolved: Int, widthOut: Int, useCsd: Boolean) =
    Bcm(constant, MsbMultiplier, widthIn, widthInvolved, widthOut, useCsd)
}

object LsbBcm {
  def apply(constant: BigInt, widthIn: Int, widthOut: Int, useCsd: Boolean) =
    Bcm(constant, LsbMultiplier, widthIn, widthOut, widthOut, useCsd)
}