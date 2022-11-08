//package Chainsaw.arithmetic
//
//import spinal.core._
//
//import scala.language.postfixOps
//import Chainsaw._
//
//case class OperandInfo(width: Int, weight: Int, positive: Boolean, time: Int) {
//  def evaluate(data: BigInt): BigInt = {
//    require(data.bitLength <= width)
//    (data << weight) * (if (positive) 1 else -1)
//  }
//
//  def maxValue: BigInt = if (positive) ((BigInt(1) << width) - 1) << weight else BigInt(0)
//
//  def <<(shiftLeft: Int) = OperandInfo(width, weight + shiftLeft, positive, time)
//
//  def >>(shiftRight: Int) = <<(-shiftRight)
//
//  def unary_- = OperandInfo(width, weight, !positive, time)
//}
//
///** implement big constant multiplication by compressor tree
// *
// * @see [[Csd]]
// */
//case class Bcm(constant: BigInt, widthIn: Int, multiplierType: MultiplierType, widthTake: Int = -1, useCsd: Boolean = false)
//  extends ChainsawGenerator {
//
//  override def name = s"bcm_${constant.hashCode()}_${targetSlice.start}_until_${targetSlice.end}".replace("-", "N")
//
//  /** --------
//   * requirements
//   * -------- */
//  if (widthTake <= 0) require(multiplierType == FullMultiplier, "widthTake must be specified for LSB/MSB mode")
//
//  /** --------
//   * width calculation
//   -------- */
//  val widthFull = constant.bitLength + widthIn
//  val widthDrop = widthFull - widthTake
//  val widthOut = if (multiplierType == FullMultiplier) widthFull else widthTake
//  val targetSlice = multiplierType match {
//    case FullMultiplier => 0 until widthFull
//    case MsbMultiplier => widthFull - widthTake until widthFull
//    case LsbMultiplier => 0 until widthTake
//  }
//
//  /** --------
//   * I/O definition
//   -------- */
//  override var inputTypes = Seq(UIntInfo(widthIn))
//  override var outputTypes = Seq.fill(2)(UIntInfo(widthOut))
//
//  override var inputFormat = inputNoControl
//  override var outputFormat = outputNoControl
//
//  /** --------
//   * operands construction
//   *  -------- */
//  // get digits of the constant, low to high
//  val constantDigits: String = (if (useCsd) Csd(constant).csd else constant.toString(2)).reverse
//
//  logger.info(s"constant digits for BCM: $constantDigits")
//
//  val sliceAndInfos: Seq[(IndexedSeq[Int], OperandInfo)] =
//    constantDigits.zipWithIndex // digit and its weight
//      .filterNot(_._1 == '0') // skip 0s
//      .map { case (c, weight) =>
//        val range = weight until weight + widthIn
//        val inter = range intersect targetSlice // get slices by intersection, this may be empty
//        val dataSlice = inter.map(_ - weight)
//        (dataSlice, OperandInfo(dataSlice.length, weight, c == '1', 0))
//      }
//      .filterNot(_._1.isEmpty) // skip empty slices
//      .map { case (slice, info) => (slice, info << slice.head) } // true weight
//
//  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
//    val data = dataIn.asInstanceOf[Seq[BigInt]].head
//
//    // coarse model
//    //    val ret = multiplierType match {
//    //      case datenlord.FullMultiplier => data * constant // A
//    //      case datenlord.MsbMultiplier => constant * data / (BigInt(1) << widthDrop) // A_high
//    //      case datenlord.LsbMultiplier => constant * data % (BigInt(1) << widthTake) // A_low
//    //    }
//
//    // accurate model
//    val raw = sliceAndInfos.map { case (slice, info) =>
//      val segment = data.toBitValue()(slice.head to slice.last)
//      val abs = segment << info.weight
//      if (info.positive) abs else -abs
//    }.sum
//
//    val ret = multiplierType match {
//      case FullMultiplier => raw
//      case MsbMultiplier => raw >> widthDrop
//      case LsbMultiplier => raw % (BigInt(1) << widthTake)
//    }
//
//    Seq(ret) // to match the output size
//  }
//
//  /** --------
//   * compressorTree declaration
//   -------- */
//  val compressorGen = CompressorTree(sliceAndInfos.map(_._2))
//  val compensation = compressorGen.compensation
//
//  /** --------
//   * metric
//   * -------- */
//  val frameMetric = (yours: Seq[Any], golden: Seq[Any]) => {
//    val target = golden.asInstanceOf[Seq[BigInt]].sum
//    val det = multiplierType match {
//      case FullMultiplier =>
//        val ret = yours.asInstanceOf[Seq[BigInt]].sum - compensation
//        ret == target
//      case LsbMultiplier =>
//        val ret = yours.asInstanceOf[Seq[BigInt]].sum - compensation
//        val modulus = BigInt(1) << widthTake
//        (ret + modulus) % modulus == target
//      case MsbMultiplier =>
//        val ret = yours.asInstanceOf[Seq[BigInt]].sum - (compensation >> widthTake)
//        logger.info(s"compensation: ${compensation >> widthTake}")
//        ret == target
//    }
//    if (!det) {
//      logger.info(s"constant = ${constant}")
//      logger.info(s"slices = ${sliceAndInfos.mkString("\n")}")
//      logger.info(s"yours = ${yours.mkString(" ")}")
//      logger.info(s"${yours.asInstanceOf[Seq[BigInt]].sum} - ${compensation} = ${yours.asInstanceOf[Seq[BigInt]].sum - compensation} != $target")
//      logger.info(s"target width = ${target.bitLength}")
//      logger.info(s"input = ${target / constant}")
//    }
//    det
//  }
//
//  val metric = ChainsawMetric(frameWise = frameMetric)
//
//  /** --------
//   * error bound analysis
//   * -------- */
//
//  val widthCoeff = constantDigits.length
//  var upperBound, lowerBound, dataForUpper, dataForLower = BigInt(0)
//  if (multiplierType == MsbMultiplier) {
//    (0 until widthIn).foreach { i => // accumulate the error bit by bit(low to high), as they are independent
//      val widthDropped = (widthDrop - i) min widthCoeff max 0
//      val constantDropped = // the constant which a bit should have been multiplied by
//        if (useCsd) Csd(constantDigits).takeLow(widthDropped).evaluate
//        else BigIntUtil(constant).takeLow(widthDropped)
//
//      if (constantDropped >= BigInt(0)) {
//        upperBound += constantDropped << i
//        dataForUpper += BigInt(1) << i
//      } else {
//        lowerBound += constantDropped << i
//        dataForLower += BigInt(1) << i
//      }
//    }
//    upperBound = (upperBound >> widthDrop) + 1
//    lowerBound = lowerBound >> widthDrop
//
//    logger.info(
//      s"\n----error analysis for big constant multiplier at MSB mode----" +
//        s"\n\terror bound of MSB multiplication: [$lowerBound, $upperBound]" +
//        s"\n\tlower bound achieved by $dataForLower" +
//        s"\n\tupper bound achieved by $dataForUpper")
//  }
//
//  override var latency = compressorGen.latency
//
//  logger.info(s"BCM latency of ${constant.bitLength} bits X $widthIn bits is $latency")
//
//  override def implH: ChainsawModule = new ChainsawModule(this) {
//
//    val data = uintDataIn.head
//    val operands = sliceAndInfos.map(_._1) // slices
//      .map { slice => data(slice.last downto slice.head) }
//
//    val core = compressorGen.getImplH
//    core.dataIn := operands.map(_.asBits)
//
//    val ret = multiplierType match {
//      case datenlord.MsbMultiplier => core.dataOut.map(_ >> widthDrop).map(_.resize(widthOut))
//      case _ => core.dataOut.map(_.resize(widthOut))
//    }
//
//    dataOut := ret
//  }
//}
//
