package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._
import Chainsaw.edaFlow._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps
import scala.util.Random

/**
  * Constant multiplier for fixed point numbers
  * @param signedIn
  * 0=unsigned, 1=signed
  * @param integralIn
  * Input integer bit width(including sign bit)
  * @param fractionalIn
  * Input fractional bit width
  * @param fractionalOut
  * Output fractional bit width
  * @param constant
  * Input constant of type double
  */
case class FixRealKCM (
  override  val family: XilinxDeviceFamily,
  override  val targetFrequency: HertzNumber,
  signedIn: Boolean,
  integralIn: Int,
  fractionalIn: Int,
  fractionalOut: Int,
  constant: Double  // positive
) extends FlopocoOperator(family, targetFrequency) {

  require(fractionalOut>=fractionalIn)

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName = "FixRealKCM"
  override val entityName = operatorName
  override val params = Seq(("signedIn", signedIn), ("msbIn", integralIn-1), ("lsbIn", -fractionalIn), ("lsbOut", -fractionalOut), ("constant", constant))

  override lazy val moduleName: String = s"FixRealKCM_F${targetFrequency.toInt/1e6.toInt}_uid2"

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = Seq(NumericType(integralIn, fractionalIn, signed = signedIn))

  val integralOut = log2Up(inputTypes.head.maxValue.toInt+1) // TODO:Not necessarily true
  override def outputTypes: Seq[NumericType] = Seq(NumericType(integralOut, fractionalOut, signed = signedIn))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(testCase.data.head*constant)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = true  // TODO

  override def testCases: Seq[TestCase] = Seq.fill(100)(TestCase(randomDataVector))

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X = in Bits (integralIn+fractionalIn+(if(signedIn) 1 else 0)  bits)
      val R = out Bits (integralOut+fractionalOut+(if(signedIn) 1 else 0)  bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X := flowIn.fragment.head.asBits
    flowOut.fragment.head.assignFromBits(box.R)
  }

  override def implNaiveH = ???
}
