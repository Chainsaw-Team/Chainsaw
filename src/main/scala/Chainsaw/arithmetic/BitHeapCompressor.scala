package Chainsaw.arithmetic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

import Chainsaw._

case class BitHeapCompressor(operandInfos: Seq[ArithInfo]) extends ChainsawGenerator {

  override def name = s"BitHeapCompressor_${operandInfos.hashCode()}".replace('-', 'N')

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts.zip(operandInfos).map { case (int, info) => (int << info.weight) * (if (info.isPositive) 1 else -1) }.sum
    Seq(ret, BigInt(0))
  }

  val (retBitHeap, solutions) = BitHeap.getHeapFromInfos[Int](Seq(operandInfos)).compressAll(Gpcs(), name = "compressor tree for config")
  val (csaLatency, widthOut) = (solutions.getLatency, if (solutions.getFinalWidthOut != 0) solutions.getFinalWidthOut else retBitHeap.width + retBitHeap.weightLow)

  // for negative operands, we use its bitwise inversion instead, and make compensation by the final CPA
  // example: -1010 = 0101 - 1111, compensation = -1111
  val compensation = {
    val negatives = operandInfos.filterNot(_.isPositive).map(info => ((BigInt(1) << info.width) - 1) << info.weight)
    (negatives :+ BigInt(0)).sum // in case of empty
  }

  def carrySaveMetric(compensation: BigInt) = ChainsawMetric(
    frameWise = (yours: Seq[Any], golden: Seq[Any]) => {
      val g = golden.asInstanceOf[Seq[BigInt]].sum
      val y = yours.asInstanceOf[Seq[BigInt]].sum
      if (g < 0) true else g == y - compensation
    }
  )

  override val metric = carrySaveMetric(compensation)

  override var inputTypes = operandInfos.map(info => UIntInfo(info.width))
  override var outputTypes = Seq.fill(2)(UIntInfo(widthOut))

  override val inputTimes = Some(operandInfos.map(_.time))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = operandInfos.map(_.time).min + csaLatency + 1

  override def implH: ChainsawModule = new ChainsawModule(this) {

    def pipeline(data: Bool): Bool = data.d()

    def zero(): Bool = False

    val operands = uintDataIn.zip(operandInfos)
      .map { case (int, info) => if (info.isPositive) int else ~int }.map(_.d().asBools)
    val heapIn = BitHeap.getHeapFromInfos(Seq(operandInfos), Seq(operands))
    val heapOut = heapIn.implCompressTree(Gpcs(), solutions, pipeline, s"operands of CompressorTree_${operandInfos.hashCode()}".replace('-', 'N'))
    val rows = heapOut.output(zero).map(_.asBits().asUInt)
    // TODO: reconsider resize - when redundant compressors are used
    uintDataOut := Seq(rows.head @@ U(0, heapOut.weightLows.head bits), rows.last @@ U(0, heapOut.weightLows.head bits))
  }
}