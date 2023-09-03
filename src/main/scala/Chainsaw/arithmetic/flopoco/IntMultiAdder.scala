package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow._
import Chainsaw.xilinx._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps
import scala.util.Random

/** bitHeap compressor implemented by FloPoCo, compressing a n X widthIn rectangle into single row
 *
 * @param signed signedness of input integers
 */
case class IntMultiAdder(override val family: XilinxDeviceFamily, override val targetFrequency: HertzNumber, widthIn: Int, n: Int, signed: Boolean)
  extends FlopocoOperator(family, targetFrequency) {

  override val operatorName: String = "IntMultiAdder"
  override val entityName: String = "IntMultiAdder"
  override val params: Seq[(String, Any)] = Seq(("signedIn", if (signed) 1 else 0), ("n", n), ("wIn", widthIn))

  val widthOut = widthIn + log2Up(n)

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X = in Vec(Bits(widthIn bits), n)
      X.zipWithIndex.foreach { case (int, i) => int.setName(s"X$i") }
      val R = out Bits (widthOut bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X := flowIn.fragment.map(_.asBits)
    flowOut.fragment.head.assignFromBits(box.R)
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = ???

  /** -------- performance
   * --------
   */
  override def vivadoUtilEstimation: VivadoUtil = ???


  /** -------- interfaces
   * --------
   */
  override def inputTypes: Seq[NumericType] =
    if (!signed) Seq.fill(n)(NumericType.U(widthIn))
    else Seq.fill(n)(NumericType.S(widthIn - 1))

  override def outputTypes: Seq[NumericType] =
    if (!signed) Seq(NumericType.U(widthOut))
    else Seq(NumericType.S(widthOut - 1))

  /** -------- model
   * --------
   */
  //  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(testCase.data.sum)
  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(testCase.data.head)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = {
    val getVector = if (!signed) Seq.fill(n)(BigInt(widthIn, Random)).map(BigDecimal(_))
    else Seq.fill(n)(BigInt(widthIn, Random) - (BigInt(1) << (widthIn - 1))).map(BigDecimal(_))
    Seq.fill(100)(TestCase(getVector))
  }
}