package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class IntMultiplier(wX: Int, wY: Int, maxDSP: Int)
    extends FlopocoOperator {
  override def implNaiveH = None

  override def vivadoUtilEstimation = VivadoUtil(dsp = maxDSP)

  override def inputTypes = Seq(wX, wY).map(NumericType.U)

  override def outputTypes = Seq(wX + wY).map(NumericType.U)

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase) = Seq(testCase.data.product)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)

  override def testCases = Seq.fill(100)(TestCase(randomDataVector))

  /** -------- params for Flopoco generation
    * --------
    */
  override def name = s"${operatorName}_${wX}_${wY}_$maxDSP"

  override val operatorName = "IntMultiplier"
  override val family       = UltraScale

  override def fmaxEstimation = 600 MHz

  override val params = Seq(("wX", wX), ("wY", wY), ("maxDSP", maxDSP))

  /** black box used in synthesis
    */
  override def blackbox = new FlopocoBlackBoxWithClk {

    val X = in Bits (wX bits)
    val Y = in Bits (wY bits)
    val R = out Bits ((wX + wY) bits)

    override def mapChainsawModule(
        flowIn: Flow[Fragment[Vec[AFix]]],
        flowOut: Flow[Fragment[Vec[AFix]]]
    ): Unit = {
      X                   := flowIn.fragment(0).asBits
      Y                   := flowIn.fragment(1).asBits
      flowOut.fragment(0) := R.asUInt.toAFix
    }
  }
}

object IntMultiplier {
  def main(args: Array[String]): Unit = {
    ChainsawSynth(IntMultiplier(wX = 32, wY = 48, maxDSP = 4))
  }
}
