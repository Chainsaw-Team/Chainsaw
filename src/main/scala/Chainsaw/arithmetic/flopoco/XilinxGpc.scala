package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.Device.Series7
import Chainsaw.edaFlow.vivado._
import spinal.core._
import spinal.lib.{Flow, Fragment}
import Chainsaw.edaFlow._

import scala.language.postfixOps

/** general parallel counter using Xilinx primitives
  * @param targetFrequency
  * @param columnHeights
  * @param outputWidth
  */
case class XilinxGpc(override val targetFrequency: HertzNumber, columnHeights: Seq[Int], outputWidth: Int)
    extends FlopocoOperator(Series7, targetFrequency) {

  // FIXME: need Xilinx UNISIM to be compiled
  // TODO: implement def impl
  // TODO: implement unit test
  // TODO: implement def implNaiveH, this is important for verification as primitives may consume too much compilation time

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName: String       = "XilinxGPC"
  override val entityName: String         = "XilinxGPC"
  override val params: Seq[(String, Any)] = Seq(("columnHeights", columnHeights.mkString(",")))

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = false) {
      // setting I/O for black box
      val Xs = columnHeights.zipWithIndex
        .filter(_._1 > 0)
        .map { case (width, weight) =>
          val ret = in Bits (width bits)
          ret.setName(s"X$weight")
          ret
        }
      val R = out Bits (outputTypes.head.bitWidth bits)
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    flowIn.fragment.zip(box.Xs).foreach { case (fix, bits) => bits := fix.asBits }
    flowOut.fragment(0) := box.R.asUInt.toAFix
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = ???

  /** -------- interfaces
    * --------
    */
  override def inputTypes = columnHeights.filter(_ > 0).map(NumericType.U)

  override def outputTypes = Seq(NumericType.U(outputWidth))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = ???

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = ???

}
