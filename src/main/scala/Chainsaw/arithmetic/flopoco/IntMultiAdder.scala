package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.Device.{UltraScale, XilinxDeviceFamily}
import Chainsaw.edaFlow.vivado._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps

/** A component adding n integers, bithead based. If wIn=1 it is also a population count.
  *
  * @param widthIn
  *   input size in bits
  * @param n
  *   number of inputs to add
  * @param signed
  *   whether inputs are signed
  */
case class IntMultiAdder(
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    widthIn: Int,
    n: Int,
    signed: Boolean
) extends FlopocoOperator(family, targetFrequency) {

  override val operatorName: String       = "IntMultiAdder"
  override val entityName: String         = "IntMultiAdder"
  override val params: Seq[(String, Any)] = Seq(("signedIn", if (signed) 1 else 0), ("n", n), ("wIn", widthIn))

  val widthOut = widthIn + log2Up(n)

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X = in Vec (Bits(widthIn bits), n)
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
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

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
  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(testCase.data.sum)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = Seq.fill(100)(TestCase(randomDataVector))

}

object IntMultiAdder {
  def main(args: Array[String]): Unit = {
    println(sys.env.contains("FLOPOCO"))
    val test = IntMultiAdder(UltraScale, 50 MHz, 8, 8, true)
    println(test.moduleName)
  }
}
