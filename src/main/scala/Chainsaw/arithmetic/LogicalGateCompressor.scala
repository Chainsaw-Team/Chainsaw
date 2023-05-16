package Chainsaw.arithmetic

import Chainsaw.xilinx.VivadoUtil
import Chainsaw._
import Chainsaw.device.LUT2
import spinal.core._
import spinal.lib.StreamFifo

import scala.math._

abstract class LogicalGateCompressor(inPortNumber: Int, outPortNumber: Int = 1) extends CompressorGenerator {

  override def inputFormat: Seq[Int] = Seq(inPortNumber)

  override def outputFormat: Seq[Int] = Seq(outPortNumber)

  override def name: String = className(this)

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = Seq.fill(inPortNumber)(NumericType.Bool())

  override def outputTypes: Seq[NumericType] = Seq.fill(outPortNumber)(NumericType.Bool())

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = {
    yours.zip(golden).forall { case (y, g) => y.toInt.toBoolean == g.toInt.toBoolean }
    yours.forall(_ == 2)
    yours.forall(a => a == 2)
  }

  def resourceCost: Int

  def performanceCost: Int
}

case class AND(override val complementHeap: Seq[Seq[Boolean]] = null) extends LogicalGateCompressor(inPortNumber = 2) {

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val Seq(a, b) = dataIn.map(_.asBits.asBool)
    val and       = LUT2(BigInt("1000", 2))
    and.I0                     := a
    and.I1                     := b
    dataOut.head.asBits.asBool := and.O
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = Some(new ChainsawOperatorModule(this) {
    val Seq(a, b) = dataIn.map(_.asBits.asBool)
    dataOut.head.asBits.asBool := a && b
  })

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(testCase.data.product)

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil(lut = 1, ff = 1)

  override def resourceCost: Int = 6

  override def performanceCost: Int = 7
}

case class OR(override val complementHeap: Seq[Seq[Boolean]] = null) extends LogicalGateCompressor(inPortNumber = 2) {

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val Seq(a, b) = dataIn.map(_.asBits.asBool)
    val and       = LUT2(BigInt("1110", 2))
    and.I0                     := a
    and.I1                     := b
    dataOut.head.asBits.asBool := and.O
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = Some(new ChainsawOperatorModule(this) {
    val Seq(a, b) = dataIn.map(_.asBits.asBool)
    dataOut.head.asBits.asBool := a || b
  })

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(
    BigDecimal(testCase.data.map(_.toInt.toBoolean).reduce(_ || _).toInt)
  )

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil(lut = 1, ff = 1)

  override def resourceCost: Int = 6

  override def performanceCost: Int = 7
}

case class XOR(override val complementHeap: Seq[Seq[Boolean]] = null) extends LogicalGateCompressor(inPortNumber = 2) {

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val Seq(a, b) = dataIn.map(_.asBits.asBool)
    val and       = LUT2(BigInt("0110", 2))
    and.I0                     := a
    and.I1                     := b
    dataOut.head.asBits.asBool := and.O
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = Some(new ChainsawOperatorModule(this) {
    val Seq(a, b) = dataIn.map(_.asBits.asBool)
    dataOut.head.asBits.asBool := a ^ b
  })

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(
    BigDecimal(testCase.data.map(_.toInt.toBoolean).reduce(_ ^ _).toInt)
  )

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil(lut = 1, ff = 1)

  override def resourceCost: Int = 12

  override def performanceCost: Int = 15
}

case class Compressor3_2(override val complementHeap: Seq[Seq[Boolean]] = null)
    extends LogicalGateCompressor(inPortNumber = 3) {

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val Seq(a, b, c) = dataIn.map(_.asBits.asBool)
    val ands         = Seq.fill(2)(AND().implH)
    val xors         = Seq.fill(2)(XOR().implH)
    val or           = OR().implH
    ands.head.dataIn := Vec(a.asUInt.toAFix, b.asUInt.toAFix)               // a && b
    xors.head.dataIn := Vec(a.asUInt.toAFix, b.asUInt.toAFix)               // a ^ b
    xors.last.dataIn := Vec(xors.head.dataOut.head, c.asUInt.toAFix)        // s = a ^ b ^ c
    ands.last.dataIn := Vec(xors.head.dataOut.head, c.asUInt.toAFix)        // c && (a ^ b)
    or.dataIn        := Vec(ands.head.dataOut.head, ands.last.dataOut.head) // c = a && b + c && (a ^ b)

    dataOut := Vec(xors.last.dataOut.head, or.dataIn.head)
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = Some(new ChainsawOperatorModule(this) {
    val Seq(a, b, c) = dataIn.map(_.asBits.asBool)
    dataOut := Vec((a ^ b ^ c).asUInt.toAFix, ((a && b) || (c && (a ^ b))).asUInt.toAFix)
  })

  override def impl(testCase: TestCase): Seq[BigDecimal] = Seq(
    BigDecimal(testCase.data.map(_.toInt.toBoolean).reduce(_ ^ _).toInt),
    BigDecimal {
      val booleans = testCase.data.map(_.toInt.toBoolean)
      ((booleans.head && booleans(2)) || (booleans.last && (booleans.head ^ booleans(2)))).toInt
    }
  )

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil(lut = 1, ff = 2)

  override def resourceCost: Int = 42

  override def performanceCost: Int = 51
}
