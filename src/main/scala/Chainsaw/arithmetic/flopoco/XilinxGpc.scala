package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

// TODO: behavior model & black box of Xilinx GPC
case class XilinxGpc(columnHeights:Seq[Int]) extends FlopocoOperator{

  override def implNaiveH = ???

  override def vivadoUtilEstimation = ???

  override def inputTypes = ???

  override def outputTypes = ???

  /** --------
   * model
   * -------- */
  override def impl(testCase: TestCase) = ???

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = ???

  override def testCases = ???

  /** --------
   * params for Flopoco generation
   * -------- */
  override def name = s"${operatorName}_${columnHeights.mkString("_")}"

  override val operatorName = "XilinxGPC"
  override val family = Series7

  override def fmaxEstimation = 600 MHz

  override val params = Seq(("columnHeights", columnHeights.mkString(",")))

  /** black box used in synthesis
   */
  override def blackbox = ???
}