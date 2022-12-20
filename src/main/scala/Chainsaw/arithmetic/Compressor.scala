package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

trait Compressor {

  /** -------- definition
    * --------
    */

  def inputFormat: Seq[Int]

  def outputFormat: Seq[Int]

  def compress(bitsIn: BitHeapHard): BitHeapHard

  /** -------- attributes
    * --------
    */
  def inputBitsCount = inputFormat.sum

  def outputBitsCount = outputFormat.sum

  def inputMax = inputFormat.zipWithIndex.map { case (w, i) => w * (BigInt(1) << i) }.sum

  def outputMax = outputFormat.zipWithIndex.map { case (w, i) => w * (BigInt(1) << i) }.sum

  def compact = inputMax == outputMax

  def redundant = !compact

  def clbCost(considerFF: Boolean = true) = vivadoUtilEstimation.cost(considerFF)

  def bitReduction: Int = inputBitsCount - outputBitsCount

  def heightReduction: Int = inputFormat.max - outputFormat.max

  def reductionRatio: Double = inputBitsCount.toDouble / outputBitsCount

  def reductionEfficiency(considerFF: Boolean = true): Double = bitReduction / clbCost(considerFF)

  def vivadoUtilEstimation: VivadoUtil

  def fmaxEstimation: HertzNumber = 600 MHz // target for all compressors

  def latency = 0

  // visualization
  override def toString() = {
    val dotsIn    = BitHeaps.getHeapFromHeights(Seq(inputFormat), Seq(0), Seq(0)).toString
    val dotsOut   = BitHeaps.getHeapFromHeights(Seq(outputFormat), Seq(0), Seq(0)).toString
    val length    = outputFormat.length
    val arrowLine = s"${" " * (length / 2) * 2}$downArrow"
    val shiftedDotsIn =
      dotsIn.split("\n").head + "\n" + dotsIn.split("\n").tail.map(_.padToLeft(length * 2 - 1, ' ')).mkString("\n")
    s"$shiftedDotsIn\n$arrowLine\n$dotsOut"
  }

}

trait CompressorGenerator extends ChainsawOperatorGenerator with Compressor {
  def columns2Infos(columns: Seq[Int]) = {
    (0 until columns.max).map(i => ArithInfo(width = columns.count(_ > i), weight = columns.indexWhere(_ > i)))
  }

  def columns2Operands(columns: Seq[Seq[Bool]]): Seq[AFix] = {
    val intColumns = columns.map(col => col.map(e => 1).sum)
    (0 until intColumns.max).map { i => (columns.filter(_.length > i).map(col => col(i)).asBits().asUInt << intColumns.indexWhere(_ > i)).toAFix }
  }

  def operands2Columns(operands: Seq[AFix], operandsFormat: Seq[Int]): Seq[Seq[Bool]] = {
    val infos   = columns2Infos(operandsFormat)
    val width   = infos.map(info => info.high + 1).max
    val columns = ArrayBuffer.fill(width)(ArrayBuffer[Bool]())
    operands.map(_.asBits).zip(infos).foreach { case (bits, info) =>
      val bitWidth = bits.getBitsWidth
      require(bitWidth == info.width, s"operand width mismatch, operand: $bitWidth, format: ${info.width}")
      (0 until bitWidth).foreach { bw => columns(bw + info.weight) += bits(bw) }
    }
    columns
  }
}
