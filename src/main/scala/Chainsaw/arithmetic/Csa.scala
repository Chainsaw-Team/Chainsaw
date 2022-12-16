package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._

/** generalized multi-input carry-save adder for any shape, based on GPCs and row adders
 *
 * @param arithInfos operand information, including width, weight, signedness and timing
 */
case class Csa(arithInfos: Seq[ArithInfo]) extends UnsignedMerge {

  // TODO: make sure that same infos lead to same names
  override def name = s"Merge_${hashName(arithInfos)}"

  override val inputTimes = arithInfos.map(_.time)
  override val outputTimes = Seq(0)

  // TODO: implementation
  // TODO: considering separating CSA and CPA
  override def implH = null

  override def latency() = inputInterval + 1

  override def vivadoUtilEstimation = ???

  override def fmaxEstimation = ???

  def sum(data: Seq[UInt]) = {
    val core = getImplH
    core.dataIn.zip(data).foreach { case (in, data) => in := data.toAFix }
    core.dataOut.head.asUInt()
  }
}
