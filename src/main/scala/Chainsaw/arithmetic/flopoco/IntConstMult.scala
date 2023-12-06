package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.Device.XilinxDeviceFamily
import Chainsaw.edaFlow.vivado._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps

/** Integer multiplier of an unsigned number by a constant using a shift-and-add tree
  *
  * @param wIn
  *   input size in bits
  * @param const
  *   constant to multiply by
  */
case class IntConstMult(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wIn: Int,
    const: Int
) extends FlopocoOperator(family, targetFrequency) {

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName: String       = "IntConstMult"
  override val entityName: String         = "IntConstMult"
  override val params: Seq[(String, Any)] = Seq(("wIn", wIn), ("n", const))

  val wOut = log2Up((pow2(wIn) - 1) * const + 1)
  override def implH = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      val X = in Bits (wIn bits)
      val R = out Bits (wOut bits)
    }
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
  override def impl(testCase: TestCase) = Seq(testCase.data.head * const)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = yours.equals(golden)

  override def testCases: Seq[TestCase] = Seq.fill(1000)(TestCase(randomDataVector))
}
