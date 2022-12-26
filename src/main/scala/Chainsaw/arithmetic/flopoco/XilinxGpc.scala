package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib.{Flow, Fragment}

import scala.language.postfixOps

// TODO: behavior model & black box of Xilinx GPC
case class XilinxGpc(columnHeights: Seq[Int], outputWidth: Int) extends FlopocoOperator {

  override def implNaiveH = ???

  override def vivadoUtilEstimation = ???

  override def inputTypes = columnHeights.filter(_ > 0).map(NumericType.U)

  override def outputTypes = Seq(NumericType.U(outputWidth))

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
  override def blackbox = new FlopocoBlackBox {

    val Xs = columnHeights.zipWithIndex.filter(_._1 > 0)
      .map { case (width, weight) =>
        val ret = in Bits (width bits)
        ret.setName(s"X$weight")
        ret
      }
    val R = out Bits (outputTypes.head.bitWidth bits)

    override def mapChainsawModule(flowIn: Flow[Fragment[Vec[AFix]]], flowOut: Flow[Fragment[Vec[AFix]]]): Unit = {
      flowIn.fragment.zip(Xs).foreach { case (fix, bits) => bits := fix.asBits }
      flowOut.fragment(0) := R.asUInt.toAFix
    }
  }
}

object XilinxGpc extends App {
  val gpc = XilinxGpc(Seq(6, 0, 6), 5)
  gpc.latency()
  VivadoSynth(gpc.rtlPath.getParentFile, gpc.moduleName)
}