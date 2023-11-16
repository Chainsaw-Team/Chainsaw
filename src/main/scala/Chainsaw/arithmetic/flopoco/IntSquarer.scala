package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.vivado._
import spinal.core._
import spinal.lib._
import Chainsaw.edaFlow._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps
import scala.util.Random

/** An integer squarer(input is unsigned).
  *
  * @param wIn
  *   size of input in bits
  */
case class IntSquarer(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wIn: Int
) extends FlopocoOperator(family, targetFrequency) {

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName               = "IntSquarer"
  override val entityName                 = "IntSquarer"
  override val params: Seq[(String, Any)] = Seq(("wIn", wIn))

  val wOut = wIn + wIn
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X = in Bits (wIn bits)
      val R = out Bits (wOut bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X := flowIn.fragment(0).asBits
    flowOut.fragment.head.assignFromBits(box.R)
  }

  override def implNaiveH = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = Seq(NumericType.U(wIn))

  override def outputTypes: Seq[NumericType] = Seq(NumericType.U(wOut))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase) = Seq(testCase.data.head * testCase.data.head)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = Seq.fill(1000)(TestCase(randomDataVector))

}
