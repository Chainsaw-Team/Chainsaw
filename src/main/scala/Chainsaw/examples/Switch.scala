package Chainsaw.examples

import Chainsaw._
import Chainsaw.edaFlow.vivado._
import spinal.core._
case class Switch() extends ChainsawOperatorGenerator with FixedLatency {
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val Seq(a, b)         = dataIn
    val Seq(aOut, bOut)   = dataOut
    def getIndex(x: AFix) = x.asBits.takeHigh(8).asUInt
    val det               = getIndex(a) > getIndex(b)
    aOut := Mux(det, a, b)
    bOut := Mux(det, b, a)
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = ???

  override def name: String = "Switch"

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = ???

  override def fmaxEstimation: HertzNumber = ???

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = Seq.fill(2)(NumericType.U(16))

  override def outputTypes: Seq[NumericType] = Seq.fill(2)(NumericType.U(16))

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = ???

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = ???

  override def testCases: Seq[TestCase] = ???

  override def latency(): Int = 0
}

object Switch {
  def main(args: Array[String]): Unit = {
    ChainsawImpl(Switch())
  }
}
