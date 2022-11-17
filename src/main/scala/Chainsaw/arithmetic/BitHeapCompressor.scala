package Chainsaw.arithmetic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

import Chainsaw._

/** enhanced multi-operand adder
 *
 * @param operandInfos operands with different width, weight, signedness and entrance time
 * @param outputAsCsa  keep output in carry-save form
 */
case class BitHeapCompressor(operandInfos: Seq[ArithInfo], outputAsCsa: Boolean) extends ChainsawGenerator {

  override def name = s"BitHeapCompressor_${operandInfos.hashCode()}".replace('-', 'N')

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts.zip(operandInfos).map { case (int, info) => (int << info.weight) * (if (info.isPositive) 1 else -1) }.sum
    if (outputAsCsa) Seq(ret, BigInt(0))
    else Seq(ret)
  }

  val initBitHeap = BitHeap.getHeapFromInfos[Int](Seq(operandInfos))
  val (retBitHeap, solutions) = initBitHeap.compressAll(Gpcs(), name = "compressor tree for config")
  val (csaLatency, widthOut) = (solutions.getLatency, if (solutions.getFinalWidthOut != 0) solutions.getFinalWidthOut else retBitHeap.width + retBitHeap.weightLow)
  val cpaGen: Option[Cpa] = if (outputAsCsa) None else Some(CpaS2S(TernarySubtractor1, widthOut, withCarry = false))
  val cpaLatency = cpaGen.map(_.latency).getOrElse(0)

  // for negative operands, we use its bitwise inversion instead, and make compensation by the final CPA
  // example: -1010 = 0101 - 1111, compensation = -1111
  val compensation = {
    val negatives = operandInfos.filterNot(_.isPositive).map(info => ((BigInt(1) << info.width) - 1) << info.weight)
    (negatives :+ BigInt(0)).sum // in case of empty
  }

  def ignoreNegativeMetric(compensation: BigInt) = ChainsawMetric(
    frameWise = (yours: Seq[Any], golden: Seq[Any]) => {
      val g = golden.asInstanceOf[Seq[BigInt]].sum
      val y = yours.asInstanceOf[Seq[BigInt]].sum
      if (g < 0) true else if (outputAsCsa) g == y - compensation else g == y
    }
  )

  override val metric = ignoreNegativeMetric(compensation)

  override var inputTypes = operandInfos.map(info => UIntInfo(info.width))
  override var outputTypes = Seq.fill(if (outputAsCsa) 2 else 1)(UIntInfo(widthOut))

  override val inputTimes = Some(operandInfos.map(_.time))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = operandInfos.map(_.time).min + csaLatency + cpaLatency + 1

  override def implH: ChainsawModule = new ChainsawModule(this) {

    //    logger.info(s"implementing bitmap compressor, height = ${initBitHeap.height}, bits = ${initBitHeap.bitsCount}, latency = $latency")
    logger.info(s"implementing bitheap compressor, compensation = $compensation")

    def pipeline(data: Bool): Bool = data.d()

    def zero(): Bool = False

    val operands = uintDataIn.zip(operandInfos)
      .map { case (int, info) => if (info.isPositive) int else ~int }.map(_.d().asBools)
    val heapIn = BitHeap.getHeapFromInfos(Seq(operandInfos), Seq(operands))
    val heapOut = heapIn.implCompressTree(Gpcs(), solutions, pipeline, s"operands of CompressorTree_${operandInfos.hashCode()}".replace('-', 'N'))
    val rows = heapOut.output(zero).map(_.asBits().asUInt).map(_ @@ U(0, heapOut.weightLow bits))
    // TODO: reconsider resize - when redundant compressors are used
    if (outputAsCsa) uintDataOut := rows
    else {
      val cpa = cpaGen.get.implH
      cpa.dataIn := (rows :+ U(compensation, widthOut bits)).map(_.asBits)
      dataOut := cpa.dataOut
    }
  }

  override def implNaiveH = Some(new ChainsawModule(this) {
    val opAndInfos = uintDataIn.zip(operandInfos)
    val positive = opAndInfos.filter(_._2.isPositive).map { case (int, info) => int << info.weight }.reduce(_ +^ _).resize(widthOut)
    val negative =
      if (operandInfos.exists(!_.isPositive))
        opAndInfos.filterNot(_._2.isPositive).map { case (int, info) => int << info.weight }.reduce(_ +^ _).resize(widthOut)
      else U(0)

    val ret = (positive - negative).d(latency)
    // FIXME: naive model of bit heap compressor when outputAsCsa
    if (outputAsCsa) null else uintDataOut.head := ret
  })
}