package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.Device.XilinxDeviceFamily
import Chainsaw.edaFlow.vivado._
import spinal.core._

import scala.language.postfixOps

/** Implements a large unsigned Multiplier using rectangular shaped tiles as appears for Xilinx FPGAs.
  *
  * @param wX
  *   size of input X
  * @param wY
  *   size of input Y
  * @param useKaratsuba
  *   Uses Karatsuba when set to 1, instead a standard tiling without sharing is used.
  * @param useRectangularTiles
  *   Uses rectangular tiles when set to 1, otherwise quadratic tiles are used.
  */
case class IntKaratsubaRectangular(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wX: Int,
    wY: Int,
    useKaratsuba: Int,
    useRectangularTiles: Int
) extends FlopocoOperator(family, targetFrequency) {

  override val operatorName = "IntKaratsubaRectangular"

  override val entityName: String = "IntKaratsubaRectangular"

  override val params =
    Seq(("wX", wX), ("wY", wY), ("useKaratsuba", useKaratsuba), ("useRectangularTiles", useRectangularTiles))

  override def inputTypes = Seq(wX, wY).map(NumericType.U)

  override def outputTypes = Seq(wX + wY).map(NumericType.U)

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase) = Seq(testCase.data.product)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = Seq.fill(100)(TestCase(randomDataVector))

  override def implH = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      val X = in Bits (wX bits)
      val Y = in Bits (wY bits)
      val R = out Bits ((wX + wY) bits)
    }
    box.X               := flowIn.fragment(0).asBits
    box.Y               := flowIn.fragment(1).asBits
    flowOut.fragment(0) := box.R.asUInt.toAFix
  }

  override def implNaiveH = ???

  override def vivadoUtilEstimation = VivadoRequirement.noRequirement

}
