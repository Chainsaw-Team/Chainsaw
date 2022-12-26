package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._
import bitheap._

import scala.language.postfixOps

/** generalized multi-input carry-save adder for any shape, based on GPCs and row adders
 *
 * @param arithInfos operand information, including width, weight, signedness and timing
 */
case class Merge(arithInfos: Seq[ArithInfo]) extends UnsignedMerge {

  // FIXME: carry-save output?
  override def outputTypes = Seq(NumericType.U(maxValue.bitLength))

  // TODO: make sure that same infos lead to same names
  override def name = s"Csa_${hashName(arithInfos)}"

  val compressorGen = BitHeapCompressor(arithInfos.map(_.toPositive))

  // TODO: implementation
  override def implH = new ChainsawOperatorModule(this) {
    ???
  }

  override def latency() = compressorGen.latency()

  override def vivadoUtilEstimation = compressorGen.vivadoUtilEstimation

  override def fmaxEstimation = 600 MHz

  def sum(data: Seq[UInt]) = {
    val core = getImplH
    core.dataIn.zip(data).foreach { case (in, data) => in := data.toAFix }
    core.dataOut.head.asUInt()
  }
}
