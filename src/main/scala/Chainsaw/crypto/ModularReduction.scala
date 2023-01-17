package Chainsaw.crypto

import Chainsaw.xilinx.VivadoUtil
import Chainsaw._
import spinal.core.{U, _}

trait ModularReductionAttribute {

  def widthM: Int

  def constantModulus: Option[BigInt]

  def upperBound: BigInt

  def widthIn = widthM + log2Up(upperBound)

  def widthOut = widthM

  def isConstant = constantModulus.isDefined

  if (isConstant)
    require(
      widthM == constantModulus.get.bitLength,
      "widthM should be equal to the bit length of the constant modulus"
    )

  def vivadoUtilEstimation: VivadoUtil

  def clbCost = vivadoUtilEstimation.lut

  def dspCost = vivadoUtilEstimation.dsp

  def report(methodName: String) = {
    s"$methodName Solution for modular reduction: " +
      s"\n\tdspCost = $dspCost, clbCost = $clbCost"
  }
}

trait ModularReduction
    extends ChainsawOperatorGenerator
    with ModularReductionAttribute
    with FixedLatency {

  override def inputTypes = {
    val widths = if (isConstant) Seq(widthIn) else Seq(widthIn, widthM)
    widths.map(NumericType.U)
  }

  override def outputTypes = Seq(NumericType.U(widthOut))

  override def impl(testCase: TestCase) = (constantModulus match {
    case Some(m) => testCase.data.map(_.toBigInt().mod(m))
    case None =>
      Seq(testCase.data(0).toBigInt().mod(testCase.data(1).toBigInt()))
  }).map(BigDecimal(_))

  override def implNaiveH =
    Some(new ChainsawOperatorModule(this) {
      val x = dataIn.head.asUInt()
      val m = constantModulus match {
        case Some(m) => U(m, widthM bits)
        case None    => dataIn.head.asUInt()
      }

      dataOut.head := (x % m).toAFix.d(latency())
    })

  // TODO: should testCases be less than M ?
  override def testCases = Seq.fill(10000)(TestCase(randomDataVector))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)
}
