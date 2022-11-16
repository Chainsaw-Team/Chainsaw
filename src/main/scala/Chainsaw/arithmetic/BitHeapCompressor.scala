package Chainsaw.arithmetic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

import Chainsaw._

case class BitHeapCompressor(operandInfos: Seq[ArithInfo], carrySaveCompress: Boolean = true) extends ChainsawGenerator {

  override def name = s"BitHeapCompressor_${operandInfos.hashCode()}".replace('-', 'N')

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts
      .zip(operandInfos)
      .map { case (int, info) => (int << info.weight) * (if (info.isPositive) 1 else -1) }
      .sum
    Seq(ret, BigInt(0))
  }

  val hasNegative           = operandInfos.exists(!_.isPositive)
  val compressTreeOutHeight = if (carrySaveCompress) 2 else 3

  // for negative operands, we use its bitwise inversion instead, and make compensation by the final CPA
  // example: -1010 = 0101 - 1111, compensation = -1111
  val compensation = {
    val negatives = operandInfos
      .filterNot(_.isPositive)
      .map(info => ((BigInt(1) << info.width) - 1) << info.weight)
    (negatives :+ BigInt(0)).sum // in case of empty
  }

  val (retBitHeap, solutions)   = BitHeaps.getHeapFromInfos[Int](Seq(operandInfos)).compressAll(Gpcs(), name = "compressor tree for config", finalHeight = compressTreeOutHeight)
  val (csaLatency, csaWidthOut) = (solutions.getLatency, if (solutions.getFinalWidthOut != 0) solutions.getFinalWidthOut else retBitHeap.width + retBitHeap.weightLow)

  val adderTypeWillUse = (carrySaveCompress, hasNegative) match {
    case (true, true)  => TernarySubtractor1
    case (false, true) => TernaryAdder
    case _             => BinaryAdder
  }

  val cpa = Cpa(
    adderTypeWillUse,
    Seq.fill((csaWidthOut - retBitHeap.weightLow) max (compensation >> retBitHeap.weightLow).bitLength)(1).grouped(96).toSeq.map(_.sum),
    S2S,
    withCarry = true
  )

  val optionalCpa = Cpa(
    BinarySubtractor,
    Seq.fill(cpa.outputWidths.head)(1).grouped(96).toSeq.map(_.sum),
    S2S,
    withCarry = true
  )

  val cpaLatency = cpa.latency

  val finalWidthOut = if (carrySaveCompress && !hasNegative) csaWidthOut else csaWidthOut + 1
  val finalLatency = (carrySaveCompress, hasNegative) match {
    case (true, true)   => csaLatency + cpaLatency + 1
    case (true, false)  => csaLatency + 1
    case (false, false) => csaLatency + cpaLatency + 1
    case (false, true)  => csaLatency + cpaLatency + optionalCpa.latency + 1
  }

  def carrySaveMetric = ChainsawMetric(
    frameWise = (yours: Seq[Any], golden: Seq[Any]) => {
      val g = golden.asInstanceOf[Seq[BigInt]].sum
      val y = yours.asInstanceOf[Seq[BigInt]].sum
      if (g < 0) true else g == y
    }
  )

  override val metric = carrySaveMetric

  override var inputTypes  = operandInfos.map(info => UIntInfo(info.width))
  override var outputTypes = Seq.fill(2)(UIntInfo(finalWidthOut))

  override val inputTimes = Some(operandInfos.map(_.time))

  override var inputFormat  = inputNoControl
  override var outputFormat = outputNoControl
  override var latency      = finalLatency

  override def implH: ChainsawModule = new ChainsawModule(this) {

    def pipeline(data: Bool): Bool = data.d()

    def zero(): Bool = False

    val operands = uintDataIn
      .zip(operandInfos)
      .map { case (int, info) => if (info.isPositive) int else ~int }
      .map(_.d().asBools)
    val heapIn  = BitHeaps.getHeapFromInfos(Seq(operandInfos), Seq(operands))
    val heapOut = heapIn.implCompressTree(Gpcs(), solutions, pipeline, s"operands of CompressorTree_${operandInfos.hashCode()}".replace('-', 'N'))
    val rows    = heapOut.output(zero, compressTreeOutHeight).map(_.asBits().asUInt)
    val ret = (carrySaveCompress, hasNegative) match {
      case (true, true) =>
        val cpaCore = cpa.getImplH
        cpaCore.dataIn := Vec(rows.head.asBits, rows.last.asBits, B(compensation >> heapOut.weightLow))
        Vec(cpaCore.dataOut.head, B(0))
      case (true, false) =>
        Vec(rows.head.asBits, rows.last.asBits)
      case (false, false) =>
        val cpaCore = cpa.getImplH
        cpaCore.flowIn.fragment := Vec(rows.head.asBits, rows(1).asBits)
        Vec(cpaCore.flowOut.fragment.head, rows.last.d(cpaLatency).asBits)
      case (false, true) =>
        val cpaCore = cpa.getImplH
        cpaCore.dataIn := Vec(rows.head.asBits, rows(1).asBits, rows.last.asBits)
        val finalCpaCore = optionalCpa.getImplH
        finalCpaCore.dataIn := Vec(cpaCore.dataOut.head, B(compensation >> heapOut.weightLow))
        Vec(finalCpaCore.dataOut.head, B(0))
    }
    uintDataOut := Seq((ret.head.asUInt @@ U(0, heapOut.weightLows.head bits)).resize(finalWidthOut), (ret.last.asUInt @@ U(0, heapOut.weightLows.head bits)).resize(finalWidthOut))
  }

}
