package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.arithmetic._
import Chainsaw.xilinx.VivadoUtil
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

/** enhanced multi-operand adder
  *
  * @param operandInfos
  *   operands with different width, weight, signedness and entrance time
  * @param outputAsCsa
  *   keep output in carry-save form
  */
case class BitHeapCompressor(operandInfos: Seq[ArithInfo], outputAsCsa: Boolean) extends ChainsawOperatorGenerator {

  override def name = getAutoName(this)

  override def impl(dataIn: TestCase): Seq[BigDecimal] = {
    val bigInts = dataIn.data
    val ret     = dataIn.data.zip(operandInfos).map { case (int, info) => (int.toBigInt() << info.weight) * (if (info.isPositive) 1 else -1) }.sum
    if (outputAsCsa) Seq(BigDecimal(ret), BigDecimal(0))
    else Seq(BigDecimal(ret))
  }

  // for negative operands, we use its bitwise inversion instead, and make compensation by the final CPA
  // example: -1010 = 0101 - 1111, compensation = -1111
  val compensation = {
    val negatives = operandInfos.filterNot(_.isPositive).map(info => ((BigInt(1) << info.width) - 1) << info.weight)
    (negatives :+ BigInt(0)).sum // in case of empty
  }

  val initBitHeap               = BitHeaps.getHeapFromInfos[Int](Seq(operandInfos))
  val bitsCount                 = initBitHeap.bitsCount
  val (retBitHeap, solutions)   = initBitHeap.compressAll(Gpcs(), name = "compressor tree for config")
  val (csaLatency, csaWidthOut) = (solutions.getLatency, if (solutions.getFinalWidthOut != 0) solutions.getFinalWidthOut else retBitHeap.width + retBitHeap.weightLow)
  val cpaWidthIn: Int           = if (outputAsCsa) 0 else (csaWidthOut max compensation.bitLength) - retBitHeap.weightLow
  val needCarryOut              = cpaWidthIn < initBitHeap.maxValue.bitLength
  val cpaGen: Option[Cpa]       = if (outputAsCsa) None else Some(Cpa(TernarySubtractor1, cpaWidthIn))
  val cpaLatency                = cpaGen.map(_.latency).getOrElse(0)

  def ignoreNegativeMetric(compensation: BigInt) = ChainsawMetric(
    frameWise = (yours: Seq[Any], golden: Seq[Any]) => {
      val g = golden.asInstanceOf[Seq[BigInt]].sum
      val y = yours.asInstanceOf[Seq[BigInt]].sum
      if (g < 0) true else if (outputAsCsa) g == y - compensation else g == y
    }
  )

  override def randomTestCase = TestCase(Seq.fill(100)(randomDataVector).flatten)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = ignoreNegativeMetric(compensation).frameWise(yours, golden)

  override def inputTypes = operandInfos.map(info => NumericType.U(info.width))

  val widthOut = if (outputAsCsa) csaWidthOut else retBitHeap.weightLow + cpaWidthIn + (if (needCarryOut) 1 else 0)

  override def outputTypes =
    if (outputAsCsa) Seq.fill(2)(NumericType.U(widthOut))
    else Seq(NumericType.U(widthOut))

  override def latency = operandInfos.map(_.time).min + csaLatency + cpaLatency + 1

  logger.info(s"---------csaLatency------------\ncsaLatency = $csaLatency")

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {

    //    logger.info(s"implementing bitmap compressor, height = ${initBitHeap.height}, bits = ${initBitHeap.bitsCount}, latency = $latency")
    logger.info(s"implementing bitheap compressor, compensation = $compensation")

    def pipeline(data: Bool): Bool = data.d()

    def zero(): Bool = False

    val operands = dataIn
      .zip(operandInfos)
      .map { case (int, info) => if (info.isPositive) int else ~int }
      .map(_.d().asUInt().asBools)

    val heapIn  = BitHeaps.getHeapFromInfos(Seq(operandInfos), Seq(operands))
    val heapOut = heapIn.implCompressTree(Gpcs(), solutions, pipeline, s"operands of CompressorTree_${operandInfos.hashCode()}".replace('-', 'N'))
    val rows    = heapOut.output(zero).map(_.asBits().asUInt)

    if (outputAsCsa) dataOut := Vec(rows.map(_ @@ U(0, heapOut.weightLow bits)).map(_.toAFix))
    else {
      val cpa = cpaGen.get.implH
      cpa.dataIn := Vec((rows :+ U(compensation >> heapOut.weightLow, cpaWidthIn bits)).map(_.asBits).map(_.asUInt.toAFix))
      dataOut    := Vec(cpa.dataOut.map(_.asUInt @@ U(0, heapOut.weightLow bits))).map(_.toAFix)
    }
  }

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    val opAndInfos = dataIn.zip(operandInfos)
    val positive   = opAndInfos.filter(_._2.isPositive).map { case (int, info) => int << info.weight }.reduce(_ +^ _)
    val negative =
      if (operandInfos.exists(!_.isPositive)) opAndInfos.filterNot(_._2.isPositive).map { case (int, info) => int << info.weight }.reduce(_ +^ _)
      else U(0).toAFix

    val ret = (positive - negative).d(latency)

    if (outputAsCsa) {
      dataOut := Vec(ret.asUInt().resize(widthOut).toAFix, U(compensation, widthOut bits).toAFix)
    } else dataOut := Vec(ret.asUInt().resize(widthOut).toAFix)
  })

  override def vivadoUtilEstimation: VivadoUtil = ???

  override def fmaxEstimation: HertzNumber = 600 MHz

  override def testCases: Seq[TestCase] = ???
}
