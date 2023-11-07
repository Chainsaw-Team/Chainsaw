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
import scala.util.control.Breaks.break

/** A leading zero counter. The output size is computed.
  *
  * @param wIn
  *   input size in bits
  * @param useLargeLut
  *   Use max unrouted lut size to build the encoding
  */
case class LZOC3(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wIn: Int,
    useLargeLut: Boolean
) extends FlopocoOperator(family, targetFrequency) {

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName = "LZOC3"
  override val entityName   = operatorName
  override val params       = Seq(("wIn", wIn), ("useLargeLut", useLargeLut))

  val wOut = log2Up(wIn + 1)
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val I         = in Bits (wIn bits)
      val countOnes = in Bool ()
      val O         = out Bits (wOut bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.I         := flowIn.fragment(0).asBits
    box.countOnes := flowIn.fragment(1).asBits.asBool
    flowOut.fragment.head.assignFromBits(box.O)
  }

  override def implNaiveH = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = Seq(NumericType.U(wIn), NumericType.U(1))

  override def outputTypes: Seq[NumericType] = Seq(NumericType.U(wOut))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = {
    def countLeadingChar(str: String, number: Char): Int = {
      var status = true
      var count  = 0
      for (ele <- str if status) {
        if (ele == number) count += 1
        else status = false
      }
      count
    }

    val inputString  = testCase.data.head.toInt.toBinaryString
    var appendString = inputString
    for (_ <- 0 until (wIn - inputString.length)) appendString = "0" + appendString

    Seq(
      BigDecimal(
        if (testCase.data.last.toInt.toBoolean) countLeadingChar(appendString, '1')
        else countLeadingChar(appendString, '0')
      )
    )
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = Seq.fill(100)(TestCase(randomDataVector))
}
