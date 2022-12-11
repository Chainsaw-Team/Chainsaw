package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

/** long multiplier implemented by divide-and-conquer
 *
 * @param bmSolution blueprint to build a big multiplier by divide-and-conquer method
 */
case class Bm(bmSolution: BmSolution) extends UnsignedMultiplier {

  def algo = BmAlgo(bmSolution)

  override val widthX = bmSolution.widthFull
  override val constant = bmSolution.constant
  override val widthY = if (isConstantMult) constant.get.bitLength else bmSolution.widthFull
  override val multiplierType = bmSolution.multiplierType
  override val widthOut =
    if (multiplierType == MsbMultiplier || multiplierType == LsbMultiplier) bmSolution.widthFull
    else bmSolution.widthOut

  override def latency() = 1

  override def name = s"${if (isConstantMult) "constant" else "variable"}_${className(multiplierType)}_${bmSolution.widthFull}"

  override def vivadoUtilEstimation = algo.vivadoUtilEstimation

  override def fmaxEstimation = 600 MHz

  override def implH = ???
}