package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.Device.{UltraScale, XilinxDeviceFamily}
import Chainsaw.edaFlow.vivado._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps
import scala.util.Random

/** General Multiplexer
  * @param wIn
  *   input word size
  * @param inputCount
  *   the number of data inputs
  */

case class GenericMux(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wIn: Int,
    inputCount: Int
) extends FlopocoOperator(family, targetFrequency) {

  override val operatorName               = "GenericMux"
  override val entityName                 = operatorName
  override val params: Seq[(String, Any)] = Seq(("wIn", wIn), ("inputCount", inputCount))

  val wOut = wIn
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X = in Vec (Bits(wIn bits), inputCount)
      X.zipWithIndex.foreach { case (int, i) => int.setName(s"iS_$i") }
      val iSel = in Bits (log2Up(inputCount) bits)
      val oMux = out Bits (wOut bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X.zipWithIndex.foreach(in => in._1 := flowIn.fragment(in._2).asBits)
    box.iSel := flowIn.fragment(box.X.length).asBits
    flowOut.fragment.head.assignFromBits(box.oMux)
  }

  override def implNaiveH = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] =
    Seq.fill(inputCount)(NumericType.U(wIn)).:+(NumericType.U(log2Up(inputCount)))

  override def outputTypes: Seq[NumericType] = Seq(NumericType.U(wOut))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(testCase.data(testCase.data.last.toInt))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = {
    def getVector: Seq[BigDecimal] = {
      var vector = Seq.fill(inputCount + 1)(BigInt(wIn, Random)).map(BigDecimal(_))
      while (vector.last >= inputCount) vector = Seq.fill(inputCount + 1)(BigInt(wIn, Random)).map(BigDecimal(_))
      vector
    }
    Seq.fill(100)(TestCase(getVector))
  }
}

object GenericMux {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(GenericMux(UltraScale, 50 MHz, 8, 5).implH)
  }
}
