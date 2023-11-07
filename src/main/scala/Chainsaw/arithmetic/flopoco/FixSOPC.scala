package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.vivado._
import spinal.core._
import spinal.lib._
import Chainsaw.edaFlow._
import breeze.linalg.InjectNumericOps
import spinal.core.{IntToBuilder, _}

import scala.Console.println
import scala.language.postfixOps
import scala.util.Random

/** A fix-point Sum of Product by Constants
  * @param lsbIn
  *   Input's last significant bit
  * @param lsbOut
  *   Output's last significant bit
  * @param coeff
  *   Input constant of type double
  */
case class FixSOPC(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    lsbIn: Int,
    lsbOut: Int,
    coeff: Double
) extends FlopocoOperator(family, targetFrequency) {

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName               = "FixSOPC"
  override val entityName                 = operatorName
  override val params: Seq[(String, Any)] = Seq(("lsbIn", lsbIn), ("lsbOut", lsbOut), ("coeff", coeff))

  override lazy val moduleName: String = s"FixSOPC_F${targetFrequency.toInt / 1e6.toInt}_uid2"

  val integralOut = log2Up(if (coeff < 1) 1 else coeff.toInt + 1)
  val fractionalOut = {
    var i = lsbOut
    while ((Math.pow(2, i) <= coeff) && (i != 0)) {
      i = i + 1
    }
    lsbOut.abs - i.abs
  }
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X0 = in Bits (lsbIn.abs + 1 bits)
      val R  = out Bits (fractionalOut + integralOut + 1 bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X0 := flowIn.fragment.head.asBits
    flowOut.fragment.head.assignFromBits(box.R)
  }

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes = Seq(NumericType.SFix(0, lsbIn.abs))

  override def outputTypes = Seq(NumericType.SFix(integralOut, fractionalOut))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(testCase.data.head * coeff)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = {
    var result       = true
    var diff: Double = 0.0
    val shift        = if (lsbOut.abs == fractionalOut) 1 else (lsbOut.abs - fractionalOut) * 2
    yours
      .zip(golden)
      .foreach(data => {
        diff                                    = (data._1 / shift - data._2).abs.toDouble
        if (diff >= Math.pow(2, lsbOut)) result = false
      })
    result
  }

  override def testCases: Seq[TestCase] = Seq.fill(100)(TestCase(randomDataVector))

  override def implNaiveH = ???
}
