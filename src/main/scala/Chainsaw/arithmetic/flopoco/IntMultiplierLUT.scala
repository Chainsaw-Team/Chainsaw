package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.Device.XilinxDeviceFamily
import Chainsaw.edaFlow.vivado._
import spinal.core._

import scala.language.postfixOps

/** Implements a LUT multiplier by simply tabulating all results in the LUT, should only be used for very small word
  * sizes(<6).
  *
  * @param wX
  *   size of input X
  * @param wY
  *   size of input Y
  */
case class IntMultiplierLUT(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wX: Int,
    wY: Int
) extends FlopocoOperator(family, targetFrequency) {

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName = "IntMultiplierLUT"
  override val entityName   = operatorName
  override val params       = Seq(("wX", wX), ("wY", wY))

  val widthOut = wX + wY
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      val X = in Bits (wX bits)
      val Y = in Bits (wY bits)
      val R = out Bits (widthOut bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X := flowIn.fragment(0).asBits
    box.Y := flowIn.fragment(1).asBits
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
  override def inputTypes: Seq[NumericType] = Seq(NumericType.U(wX), NumericType.U(wY))

  override def outputTypes: Seq[NumericType] = Seq(NumericType.U(widthOut))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(testCase.data.product)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = Seq.fill(100)(TestCase(randomDataVector))
}
