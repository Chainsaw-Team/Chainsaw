package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.Device.XilinxDeviceFamily
import Chainsaw.edaFlow.vivado._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps

/** A leading zero of one counter. The output size is computed.
  *
  * @param wIn
  *   input size in bits
  * @param countType
  *   0: count zeroes, 1 means count ones, -1 means add an input that defines what to count
  */
case class LZOC(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wIn: Int,
    countType: Int // 0:count zeros, 1:count ones, -1:add an input that define what to count
) extends FlopocoOperator(family, targetFrequency) {

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName = "LZOC"
  override val entityName = countType match {
    case 0  => "LZC"
    case 1  => "LOC"
    case -1 => operatorName
  }
  override val params = Seq(("wIn", wIn), ("countType", countType))

  val wOut = log2Up(wIn + 1)
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val I   = in Bits (wIn bits)
      val OZB = (countType == -1).generate(in Bool ())
      val O   = out Bits (wOut bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.I                        := flowIn.fragment(0).asBits
    if (countType == -1) box.OZB := flowIn.fragment(1).asBits.asBool
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
  override def inputTypes: Seq[NumericType] = countType match {
    case -1 => Seq(NumericType.U(wIn), NumericType.U(1))
    case _  => Seq(NumericType.U(wIn))
  }

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

    countType match {
      case -1 =>
        Seq(
          BigDecimal(
            if (testCase.data.last.toInt.toBoolean) countLeadingChar(appendString, '1')
            else countLeadingChar(appendString, '0')
          )
        )
      case _ =>
        if (countType == 0) Seq(BigDecimal(countLeadingChar(appendString, '0')))
        else Seq(BigDecimal(countLeadingChar(appendString, '1')))
    }
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = Seq.fill(100)(TestCase(randomDataVector))
}
