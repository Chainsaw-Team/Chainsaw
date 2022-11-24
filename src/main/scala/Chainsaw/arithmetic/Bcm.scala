package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx.VivadoUtilEstimation
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

/** implement big constant multiplication by compressor tree
 *
 * @see [[Csd]]
 * @see [[TruncatedConstantMult]]
 */
case class Bcm(constant: BigInt, multiplierType: MultiplierType, widthIn: Int, widthInvolved: Int, widthOut: Int, useCsd: Boolean = false)
  extends ChainsawGenerator {

  override def name = getAutoName(this)

  val model = TruncatedConstantMult(constant, multiplierType, widthIn, widthInvolved, widthOut, useCsd)

  import model._

  override var inputTypes = Seq(UIntInfo(widthIn))
  override var outputTypes = Seq.fill(1)(UIntInfo(widthOut))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  val compressorGen = BitHeapCompressor(sliceAndInfos.map(_._2), outputAsCsa = true)
  val cpaGen = CpaS2S(TernarySubtractor1, widthOut, withCarry = false)

  // TODO: estimation on ff
  val bitHeapEff = 1.8
  utilEstimation = VivadoUtilEstimation(lut = (compressorGen.initBitHeap.bitsCount / bitHeapEff).toInt + cpaGen.widthFull)

  override var latency = compressorGen.latency + cpaGen.latency

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val data = dataIn.asInstanceOf[Seq[BigInt]].head
    val ret = model.impl(data)
    Seq(ret)
  }

  val outputModulus = Pow2(widthOut)
  val compensation = (compressorGen.compensation >> model.widthNotOutputted).mod(outputModulus)

  val frameWiseMetric = (yours: Seq[Any], golden: Seq[Any]) => {

    val goldenData = golden.head.asInstanceOf[BigInt]
    val yourData = yours.head.asInstanceOf[BigInt] // can be implemented by a following CPA

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

  override val metric = ChainsawMetric(frameWise = frameWiseMetric)

  override def generateTestCases = Seq.fill(1000)(BigInt(widthIn, Random))

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

    val rows = multiplierType match {
      case MsbMultiplier => core.dataOut.map(_ >> widthNotOutputted).map(_.resize(widthOut))
      case _ => core.dataOut.map(_.resize(widthOut))
    }

    val cpa = cpaGen.implH
    cpa.dataIn := rows :+ B(compensation, widthOut bits)

    dataOut := cpa.dataOut
  }

  override def implNaiveH = Some(new ChainsawModule(this) {

    val x = uintDataIn.head

    val positive = sliceAndInfos.filter(_._2.isPositive)
      .map { case (slice, info) => x(slice.last downto slice.head) << info.weight }.reduce(_ +^ _)

    val negative =
      if (sliceAndInfos.exists(!_._2.isPositive))
        sliceAndInfos.filterNot(_._2.isPositive)
          .map { case (slice, info) => x(slice.last downto slice.head) << info.weight }.reduce(_ +^ _)
      else U(0)

    val raw = (positive - negative).resize(widthFull)

    val ret = multiplierType match {
      case FullMultiplier => raw
      case MsbMultiplier => raw >> widthNotOutputted
      case LsbMultiplier => raw.resize(widthInvolved)
    }

    uintDataOut.head := ret.d(latency)
  })
}

object MsbBcm {
  def apply(constant: BigInt, widthIn: Int, widthInvolved: Int, widthOut: Int, useCsd: Boolean) =
    Bcm(constant, MsbMultiplier, widthIn, widthInvolved, widthOut, useCsd)
}

object LsbBcm {
  def apply(constant: BigInt, widthIn: Int, widthOut: Int, useCsd: Boolean) =
    Bcm(constant, LsbMultiplier, widthIn, widthOut, widthOut, useCsd)
}