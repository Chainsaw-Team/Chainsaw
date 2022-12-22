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

  val bitHeap = BitHeap.fromInfos(arithInfos)
  val solution = NaiveSolver.solveAll(bitHeap)

  // FIXME: carry-save output?
  override def outputTypes = Seq(NumericType.U(maxValue.bitLength))

  // TODO: make sure that same infos lead to same names
  override def name = s"Csa_${hashName(solution)}"

  val compressorGen = BitHeapCompressor(arithInfos.map(_.toPositive))

  // TODO: implementation
  override def implH = new ChainsawOperatorModule(this) {
    val operands = dataIn.zip(arithInfos).map { case (int, info) => if (info.isPositive) int else ~int }
    val rowsOut = compressorGen.process(operands)
  }

  override def latency() = solution.latency()

  override def vivadoUtilEstimation = solution.vivadoUtilEstimation

  override def fmaxEstimation = 600 MHz

  def sum(data: Seq[UInt]) = {
    val core = getImplH
    core.dataIn.zip(data).foreach { case (in, data) => in := data.toAFix }
    core.dataOut.head.asUInt()
  }
}
