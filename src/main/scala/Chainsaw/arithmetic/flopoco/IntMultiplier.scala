package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._
import Chainsaw.edaFlow._

import scala.language.postfixOps

/** integer multiplier implemented by FloPoCo, optimized for Xilinx FPGAs
 */
case class IntMultiplier(override val family: XilinxDeviceFamily, override val targetFrequency: HertzNumber, wX: Int, wY: Int, maxDSP: Int)
  extends FlopocoOperator(family, targetFrequency) {

  override val operatorName = "IntMultiplier"

  override val entityName: String = "IntMultiplier"

  override val params = Seq(("wX", wX), ("wY", wY), ("maxDSP", maxDSP))

  override def inputTypes = Seq(wX, wY).map(NumericType.U)

  override def outputTypes = Seq(wX + wY).map(NumericType.U)

  /** -------- model
   * --------
   */
  override def impl(testCase: TestCase) = Seq(testCase.data.product)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)

  override def testCases = Seq.fill(100)(TestCase(randomDataVector))

  override def implH = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      val X = in Bits (wX bits)
      val Y = in Bits (wY bits)
      val R = out Bits ((wX + wY) bits)
    }
    box.X := flowIn.fragment(0).asBits
    box.Y := flowIn.fragment(1).asBits
    flowOut.fragment(0) := box.R.asUInt.toAFix
  }

  override def implNaiveH = ???

  override def vivadoUtilEstimation = VivadoUtil(dsp = maxDSP)


}
