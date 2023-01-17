package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._
import bitheap._

import scala.language.postfixOps

/** generalized multi-input carry-save adder for any shape, based on GPCs and
  * row adders
  *
  * @param arithInfos
  *   operand information, including width, weight, signedness and timing
  */
case class Merge(arithInfos: Seq[ArithInfo]) extends UnsignedMerge {

  override def name = s"Merge_${hashName(arithInfos)}"

  val compressorGen           = BitHeapCompressor(arithInfos)
  override val positiveLength = compressorGen.positiveLength

  val cpaGen = {
    if (compressorGen.positiveLength > cpaWidthMax) CcaAdder(positiveLength, 64)
    else if (compressorGen.outputTypes.length == 3)
      Cpa(TernaryAdder, positiveLength)
    else if (compressorGen.outputTypes.length == 2)
      Cpa(BinaryAdder, positiveLength)
    else
      throw new IllegalStateException(
        s"invalid compressor output type ${compressorGen.outputTypes.mkString(" ")}"
      )
  }

  //  override def outputTypes = Seq(NumericType.U(positiveLength))
  override def outputTypes = Seq(NumericType.U(positiveLength))

  override def outputArithInfos = Seq(
    ArithInfo(positiveLength, arithInfos.map(_.weight).min)
  )

  override def latency() = compressorGen.latency() + cpaGen.latency()

  override def vivadoUtilEstimation =
    compressorGen.vivadoUtilEstimation + cpaGen.vivadoUtilEstimation

  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawOperatorModule(this) {
    val (rows, compressorValid) = compressorGen.process(dataIn, validIn)

    val cpa = cpaGen.getImplH
    if (cpaGen.isInstanceOf[CcaAdder]) cpa.dataIn := rows :+ U(0, 1 bits).toAFix
    else cpa.dataIn                               := rows
    cpa.validIn                                   := compressorValid
    dataOut := cpa.dataOut.map(_.truncated)
  }

  def sum(data: Seq[UInt]) = process(data.map(_.toAFix)).head.asUInt()
}
