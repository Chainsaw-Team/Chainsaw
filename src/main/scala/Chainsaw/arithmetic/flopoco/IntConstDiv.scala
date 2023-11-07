package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.vivado._
import spinal.core._
import spinal.lib._
import Chainsaw.edaFlow._
import breeze.linalg.InjectNumericOps
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps
import scala.util.Random

/** Integer divider by a small constant, input only supports unsigned numbers
  * @param wIn
  *   input size in bits
  * @param divisor
  *   divisor
  * @param outputQuotient
  *   if true, output quotient
  * @param outputRemainder
  *   if true, output remainder
  * @param arch
  *   architecture used: 0 for linear-time, 1 for log-time, 2 for multiply-and-add by reciprocal
  */
case class IntConstDiv(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wIn: Int,
    divisor: Int, // TODO:design a decomposition algorithm
    outputQuotient: Boolean,
    outputRemainder: Boolean,
    arch: Int
) extends FlopocoOperator(family, targetFrequency) {

  require(outputQuotient || outputRemainder, "one must true")

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName = "IntConstDiv"
  override val entityName   = operatorName
  override val params: Seq[(String, Any)] = Seq(
    ("wIn", wIn),
    ("computeQuotient", outputQuotient),
    ("computeRemainder", outputRemainder),
    ("arch", arch),
    ("d", divisor)
  )

  val quotientBitWidth  = log2Up(scala.math.floor((scala.math.pow(2, wIn) - 1) / divisor).toInt + 1)
  val remainderBitWidth = log2Up(divisor)
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X = in Bits (wIn bits)
      val Q = outputQuotient.generate {
        val Q = out Bits (quotientBitWidth bits)
        Q
      }
      val R = outputRemainder.generate {
        val R = out Bits (remainderBitWidth bits)
        R
      }
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X := flowIn.fragment(0).asBits
    if (outputQuotient && outputRemainder) {
      flowOut.fragment(0).assignFromBits(box.Q)
      flowOut.fragment(1).assignFromBits(box.R)
    } else {
      if (outputQuotient) flowOut.fragment(0).assignFromBits(box.Q)
      if (outputRemainder) flowOut.fragment(0).assignFromBits(box.R)
    }
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

  override def outputTypes: Seq[NumericType] = {
    if (outputQuotient && outputRemainder) Seq(NumericType.U(quotientBitWidth), NumericType.U(remainderBitWidth))
    else if (outputQuotient) Seq(NumericType.U(quotientBitWidth))
    else Seq(NumericType.U(remainderBitWidth))
  }

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = {
    if (outputQuotient && outputRemainder) Seq(testCase.data.head.toInt / divisor, testCase.data.head % divisor)
    else if (outputQuotient) Seq(testCase.data.head.toInt / divisor)
    else Seq(testCase.data.head % divisor)
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = {
    Seq.fill(1000)(TestCase(randomDataVector))
  }
}
