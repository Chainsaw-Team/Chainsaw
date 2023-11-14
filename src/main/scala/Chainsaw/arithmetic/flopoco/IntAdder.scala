package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow._
import Chainsaw.edaFlow.vivado._
import spinal.core.{IntToBuilder, _}
import spinal.lib.cpu.riscv.impl.Utils.M

import scala.language.postfixOps
import scala.util.Random

/** Signed integer addition with rounding
  * @param wIn
  *   input size in bits
  */

case class IntAdder(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wIn: Int
) extends FlopocoOperator(family, targetFrequency) {

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X   = in Bits (wIn bits)
      val Y   = in Bits (wIn bits)
      val Cin = in Bool ()
      val R   = out Bits (wOut bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X   := flowIn.fragment(0).asBits
    box.Y   := flowIn.fragment(1).asBits
    box.Cin := flowIn.fragment(2).asBits.asBool
    flowOut.fragment.head.assignFromBits(box.R)
  }

  override def implNaiveH = ???

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName: String = "IntAdder"

  override val entityName: String = "IntAdder"

  override val params: Seq[(String, Any)] = Seq(("wIn", wIn))

  val wOut = wIn

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = Seq(NumericType.S(wIn - 1), NumericType.S(wIn - 1), NumericType.Bool())

  override def outputTypes: Seq[NumericType] = Seq(NumericType.S(wOut - 1))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase) = Seq(testCase.data.sum)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = {
    def getVector: Seq[BigDecimal] = {
      var X   = BigInt(wIn, Random) - (BigInt(1) << (wIn - 1))
      var Y   = BigInt(wIn, Random) - (BigInt(1) << (wIn - 1))
      var Cin = BigInt(1, util.Random)
      while ((X + Y + Cin > pow2(wOut - 1) - 1) || (X + Y + Cin < -pow2(wOut - 1))) {
        X   = BigInt(wIn, Random) - (BigInt(1) << (wIn - 1))
        Y   = BigInt(wIn, Random) - (BigInt(1) << (wIn - 1))
        Cin = BigInt(1, util.Random)
      }
      Seq(X, Y, Cin).map(BigDecimal(_))
    }
    Seq.fill(100)(TestCase(getVector))
  }
}
