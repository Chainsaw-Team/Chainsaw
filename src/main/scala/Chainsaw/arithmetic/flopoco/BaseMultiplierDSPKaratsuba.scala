package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow.vivado._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** n-split Karatsuba multiplier, whose size is wX * split X wY * split
  *
  * @param wX
  *   width of base multiplier
  * @param wY
  *   height of base multiplier
  * @param split
  *   number of splits
  */
case class BaseMultiplierDSPKaratsuba(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wX: Int,
    wY: Int,
    split: Int
) extends FlopocoOperator(family, targetFrequency) {

  val widthX = wX * split
  val widthY = wY * split
  val widthR = widthX + widthY

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName: String       = "BaseMultiplierDSPKaratsuba"
  override val entityName: String         = "IntKaratsuba"
  override val params: Seq[(String, Any)] = Seq(("wX", wX), ("wY", wY), ("n", split - 1))

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X = in Bits (widthX bits)
      val Y = in Bits (widthY bits)
      val R = out Bits (widthR bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X              := flowIn.payload(0).asBits
    box.Y              := flowIn.payload(1).asBits
    flowOut.payload(0) := box.R.asUInt.toAFix
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes = Seq(NumericType.U(widthX), NumericType.U(widthY))

  override def outputTypes = Seq(NumericType.U(widthR))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase) = Seq(testCase.data.product)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = yours.equals(golden)

  override def testCases = {
    def getVector: Seq[BigDecimal] = Seq(BigInt(widthX), BigInt(widthY)).map(BigDecimal(_))
    Seq.fill(1000)(TestCase(getVector))
  }
}
