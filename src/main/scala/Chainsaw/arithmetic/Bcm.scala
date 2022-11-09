package Chainsaw.arithmetic

import Chainsaw._
import cc.redberry.rings.scaladsl._
import spinal.core._

import scala.language.postfixOps

/** implement big constant multiplication by compressor tree
 *
 * @see [[Csd]]
 */
case class Bcm(constant: BigInt, widthIn: Int, multiplierType: MultiplierType, widthTake: Int = -1, useCsd: Boolean = false)
  extends ChainsawGenerator {

  override def name = s"bcm_${constant.hashCode()}_${targetSlice.start}_until_${targetSlice.end}".replace("-", "N")

  /** --------
   * requirements
   * -------- */
  if (widthTake <= 0) require(multiplierType == FullMultiplier, "widthTake must be specified for LSB/MSB mode")

  /** --------
   * width calculation
   * -------- */
  val widthFull = constant.bitLength + widthIn
  val targetSlice = multiplierType match {
    case FullMultiplier => 0 until widthFull
    case MsbMultiplier => widthFull - widthTake until widthFull
    case LsbMultiplier => 0 until widthTake
  }
  val widthOut = if (multiplierType == FullMultiplier) widthFull else targetSlice.length
  val baseWeight = targetSlice.low

  /** --------
   * I/O definition
   * -------- */
  override var inputTypes = Seq(UIntInfo(widthIn))
  override var outputTypes = Seq.fill(2)(UIntInfo(widthOut))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  /** --------
   * operands construction
   * -------- */
  // get digits of the constant, low to high
  val constantDigits: String = (if (useCsd) Csd(constant).csd else constant.toString(2)).reverse

  logger.info(s"constant digits for BCM: $constantDigits")

  val sliceAndInfos: Seq[(IndexedSeq[Int], ArithInfo)] =
    constantDigits.zipWithIndex // digit and its weight
      .filterNot(_._1 == '0') // skip 0s
      .map { case (c, weight) =>
        val range = weight until weight + widthIn
        val inter = range intersect targetSlice // get slices by intersection, this may be empty
        val dataSlice = inter.map(_ - weight) // inclusive slice
        (dataSlice, ArithInfo(dataSlice.length, weight, c == '1'))
      }
      .filterNot(_._1.isEmpty) // skip empty slices
      .map { case (slice, info) => (slice, info << slice.head) } // true weight

  /** --------
   * compressorTree declaration
   * -------- */
  val compressorGen = BitHeapCompressor(sliceAndInfos.map(_._2))
  override var latency = compressorGen.latency

  /** --------
   * model & metric
   * -------- */
  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val data = dataIn.asInstanceOf[Seq[BigInt]].head

    // accurate model
    val raw = sliceAndInfos.map { case (slice, info) =>
      val segment = data.toBitValue()(slice.head to slice.last)
      val abs = segment << info.weight
      if (info.isPositive) abs else -abs
    }.sum

    val ret = multiplierType match {
      case FullMultiplier => raw
      case MsbMultiplier => raw >> baseWeight
      case LsbMultiplier => raw % Pow2(widthTake)
    }

    Seq(ret, BigInt(0)) // to match the output size
  }

  val compensation = compressorGen.compensation >> baseWeight

  val frameWiseMetric = (yours: Seq[Any], golden: Seq[Any]) => {

    val goldenSum = golden.asInstanceOf[Seq[BigInt]].sum

    val yourSum = yours.asInstanceOf[Seq[BigInt]].sum - compensation // can be implemented by a following CPA

    val det = multiplierType match {
      case FullMultiplier => yourSum == goldenSum
      case MsbMultiplier =>
        yourSum == goldenSum
      case LsbMultiplier =>
        val modulus = Pow2(widthTake)
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

  /** --------
   * error bound analysis for MSB mode
   * -------- */
  val widthCoeff = constantDigits.length
  var upperBound, lowerBound, dataForUpper, dataForLower = BigInt(0)
  if (multiplierType == MsbMultiplier) {
    (0 until widthIn).foreach { i => // accumulate the error bit by bit(low to high), as they are independent
      val widthDropped = (baseWeight - i) min widthCoeff max 0
      val constantDropped = // the constant which a bit should have been multiplied by (its weight)
        if (useCsd) Csd(constantDigits).takeLow(widthDropped).evaluate
        else constant.toBitValue().takeLow(widthDropped)

      if (constantDropped >= BigInt(0)) {
        upperBound += constantDropped << i
        dataForUpper += BigInt(1) << i
      } else {
        lowerBound += constantDropped << i
        dataForLower += BigInt(1) << i
      }
    }
    upperBound = (upperBound >> baseWeight) + 1
    lowerBound = lowerBound >> baseWeight

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
      case MsbMultiplier => core.dataOut.map(_ >> baseWeight).map(_.resize(widthOut))
      case _ => core.dataOut.map(_.resize(widthOut))
    }

    dataOut := ret
  }
}

