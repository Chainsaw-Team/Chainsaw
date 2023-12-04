package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.TestCase
import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow.vivado._
import spinal.core._

import scala.math.BigDecimal.int2bigDecimal
import scala.util.Random

/** Pipelined dual adder/subtractor, output X-Y and X+Y together
  *
  * @param wIn
  *   input size in bits
  */
case class IntDualAddSub(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wIn: Int
) extends FlopocoOperator(family, targetFrequency) {

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X   = in Bits (wIn bits)
      val Y   = in Bits (wIn bits)
      val XmY = out Bits (wIn bits)
      val XpY = out Bits (wIn bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X := flowIn.fragment(0).asBits
    box.Y := flowIn.fragment(1).asBits
    flowOut.fragment(0).assignFromBits(box.XmY)
    flowOut.fragment(1).assignFromBits(box.XpY)
  }

  override def implNaiveH = ???

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName               = "IntDualAddSub"
  override val entityName                 = operatorName
  override val params: Seq[(String, Any)] = Seq(("wIn", wIn), ("opType", 1))

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = Seq(wIn - 1, wIn - 1).map(NumericType.S)

  val widthOut                               = wIn
  override def outputTypes: Seq[NumericType] = Seq(widthOut - 1, widthOut - 1).map(NumericType.S)

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] =
    Seq(testCase.data.reduce((X, Y) => X - Y), testCase.data.reduce((X, Y) => X + Y))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = {
    def inRange(width: Int, signed: Boolean, data: BigInt): Boolean = {
      val maxValue = if (signed) pow2(width - 1) - 1 else pow2(width) - 1
      val minValue = if (signed) -pow2(width - 1) else 0.toBigInt
      if ((data >= minValue) && (data <= maxValue)) true
      else false
    }

    def getVector: Seq[BigDecimal] = {
      var X = BigInt(wIn, Random) - (BigInt(1) << (wIn - 1))
      var Y = BigInt(wIn, Random) - (BigInt(1) << (wIn - 1))
      while (!(inRange(wIn, true, X - Y) && inRange(wIn, true, X + Y))) {
        X = BigInt(wIn, Random) - (BigInt(1) << (wIn - 1))
        Y = BigInt(wIn, Random) - (BigInt(1) << (wIn - 1))
      }
      Seq(X, Y).map(BigDecimal(_))
    }
    Seq.fill(100)(TestCase(getVector))
  }
}
