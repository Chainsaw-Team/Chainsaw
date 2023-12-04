package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.Device.XilinxDeviceFamily
import Chainsaw.edaFlow.vivado._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps

/** Computes sin(pi*x) and cos(pi*x) for x in -[1,1), using tables and multipliers.
  * @param lsb
  *   weight of the LSB of the input and outputs
  * @param method
  *   0 for table- and mult-based, 1 for traditional CORDIC, 2 for reduced-iteration CORDIC
  */

case class FixSinCos(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    lsb: Int,
    method: Int
) extends FlopocoOperator(family, targetFrequency) {

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName = "FixSinCos"
  override val entityName = method match {
    case 0     => "FixSinCosPoly"
    case 1 | 2 => "FixSinCosCORDIC"
  }
  override val params: Seq[(String, Any)] = Seq(("lsb", lsb), ("method", method))

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X = in Bits ((lsb.abs + 1) bits)
      val S = out Bits ((lsb.abs + 1) bits)
      val C = out Bits ((lsb.abs + 1) bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X := flowIn.fragment(0).asBits
    flowOut.fragment.head.assignFromBits(box.S)
    flowOut.fragment.last.assignFromBits(box.C)
  }

  override def implNaiveH = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes = Seq(NumericType.SFix(0, lsb.abs))

  override def outputTypes = Seq(NumericType.SFix(0, lsb.abs), NumericType.SFix(0, lsb.abs))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] =
    Seq(Math.sin(testCase.data.head.toDouble * Math.PI), Math.cos(testCase.data.head.toDouble * Math.PI))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = {
    var result       = true
    var diff: Double = 0.0
    yours
      .zip(golden)
      .foreach(data => {
        diff                                     = (data._1 - data._2).abs.toDouble
        if (diff >= Math.pow(2, lsb + 1)) result = false //when lsb=-3，Math.pow(2, lsb+1)=0.25,误差控制在+-0.25即可
      })
    result
  }

  override def testCases: Seq[TestCase] = Seq.fill(100)(TestCase(randomDataVector))
}
