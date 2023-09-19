package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._
import Chainsaw.edaFlow._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps
import scala.util.Random

/** A plain shift register implementation.
  *
  * @param wIn
  * the size of input
  * @param stages
  * the number of stages in the shift register, also the number of ouputs
  * @param resetType
  * the reset type (0 for none, 1 for synchronous, 2 for asynchronous)
  */
case class ShiftReg (
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber,
    wIn: Int,
    stages: Int,
    resetType: Int  // 0:None, 1:synchronous, 2:asynchronous
) extends FlopocoOperator(family, targetFrequency){
  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName = "ShiftReg"
  override val entityName = operatorName
  override val params = Seq(("w", wIn), ("n", stages))

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = true) {
      // setting I/O for black box
      val X = in Bits(wIn bits)
      val Xd = out Vec (Bits(wIn bits), stages)
      Xd.zipWithIndex.foreach { case (int, i) => int.setName(s"Xd${i+1}") }
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.X := flowIn.fragment.head.asBits
    flowOut.fragment.zipWithIndex.foreach{ case (afix, i) => afix.assignFromBits(box.Xd(i)) }
  }

  override def implNaiveH = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = Seq(NumericType(wIn, 0, false))

  override def outputTypes: Seq[NumericType] = for(_ <- 0 until stages) yield NumericType(wIn, 0, false)

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase) = ???

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = ???

  override def testCases = ???
}

object ShiftReg {
  def main(args: Array[String]): Unit = {
   SpinalVerilog(ShiftReg(UltraScale, 200 MHz, 8, 4, 0).implH)
  }
}
