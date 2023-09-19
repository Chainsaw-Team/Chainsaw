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
  * General-purpose lookup table, need to enter the "truth table", when the output bit width is large, the automatic complement of 0
  * @param wIn
  * input word size
  * @param wOut
  * output word size
  * @param outputValues
  * the valuse of the output, of type Sequence
  * @param lutName
  * unique name for the LUT
  */
case class GenericLut (
  override val family: XilinxDeviceFamily,
  override val targetFrequency: HertzNumber,
  wIn: Int,
  wOut: Int,
  outputValues: Seq[Int],
  lutName: String
) extends FlopocoOperator(family, targetFrequency){

  val inputValuesString = outputValues.indices.map(_.toString).reduceLeft((left, right) => left + ":" + right)
  val outputValuesString = outputValues.map(_.toString).reduceLeft((left, right) => left + ":" + right)

  /** -------- params for FloPoCo generation
    * --------
    */
  override val operatorName = "GenericLut"
  override val entityName = operatorName
  override val params = Seq(("wIn", wIn), ("wOut", wOut), ("inputValues", inputValuesString), ("outputValues", outputValuesString), ("entityName", lutName))
  override def name: String = s"${operatorName}_wIn_${wIn}_wOut_${wOut}_$lutName" // name can't include ":"

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val box = new FlopocoBlackBox(hasClk = false) {
      // setting I/O for black box
      val i = in Vec (Bits(1 bits), wIn)
      i.zipWithIndex.foreach { case (int, i) => int.setName(s"i$i") }
      val o = out Vec (Bits(1 bits), wOut)
      o.zipWithIndex.foreach { case (int, i) => int.setName(s"o$i") }
    }
    // mapping I/O of ChainsawOperatorModule to the black box
    box.i := flowIn.fragment.map(_.asBits)
    flowOut.fragment.zip(box.o).foreach(ele => ele._1.assignFromBits(ele._2))
  }

  override def implNaiveH = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoRequirement.noRequirement

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = Seq.fill(wIn)(NumericType.U(1))

  override def outputTypes: Seq[NumericType] = Seq.fill(wOut)(NumericType.U(1))

  /** -------- behavior model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = {
    val index = Integer.parseInt(testCase.data.map(_.toString()).reduceRight((left, right) => right + left), 2)
    if(index>=outputValues.length) Seq.fill(wOut)(0)
    else {
      val value = outputValues(index).toBinaryString
      var appendValue = value
      for(_ <- 0 until (wOut-value.length)) appendValue = "0" + appendValue
      for(ele <- appendValue.reverse) yield BigDecimal(ele.toString.toInt)
    }
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = yours.equals(golden)

  override def testCases: Seq[TestCase] = Seq.fill(100)(TestCase(randomDataVector))
}

