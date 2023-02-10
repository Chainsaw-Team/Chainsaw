package Chainsaw.dsp

import Chainsaw._
import Chainsaw.xilinx.VivadoUtil
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

case class FilterPrecisionNew(coeffType: NumericType, dataType: NumericType) {
  override def toString = ""
}

/** systolic fir for FPGAs, extremely efficient for Xilinx device
  */
case class Fir(
    coeffs: Seq[Double],
    coeffType: NumericType,
    dataType: NumericType,
    symmetric: Boolean = false
) extends ChainsawInfiniteGenerator
    with FixedLatency {

  if (symmetric)
    require(
      coeffs.length % 2 == 0,
      "odd tap number is not supported for symmetric mode"
    )
  val temp = dataType * coeffType
  val productType =
    if (symmetric) NumericType(temp.integral + 1, temp.fractional, temp.signed)
    else temp
  val coeffsInUse =
    if (symmetric) coeffs.take(coeffs.length.divideAndCeil(2)) else coeffs

  override def name = s"${if (symmetric) "Symmetric"
  else "Asymmetric"}_Fir_${dataType}_${coeffType}_coeff${hashName(coeffs)}"

  override def implNaiveH = None

  override def impl(testCase: TestCase) = {
    conv(testCase.data.toArray.map(_.toDouble), coeffs.toArray)
      .drop(coeffs.length - 1)
      .map(BigDecimal(_))
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    corrMetric(yours, golden, 0.9)

  override def testCases =
    Seq.fill(3)(
      TestCase(
        randomDataSequence(Random.nextInt(1000) + 1000)
          ++ Seq.fill(coeffs.length)(BigDecimal(0))
      )
    )

  override def resetCycle = latency()

  override def latency() =
    if (symmetric) 3 * (coeffsInUse.length + 1) + 1
    else 2 * (coeffsInUse.length + 1) + 1

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(productType)

  override def vivadoUtilEstimation = VivadoUtil(dsp = coeffsInUse.length)

  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawInfiniteModule(this) {
    val x = dataIn.head.asSInt()

    // using SInt inside for better inference in Quartus, as AFix multiplication do sign extension, which won't be recognized and optimized by Quartus
    // SInt datapath
    val xline = Seq.iterate(x.d(2), coeffsInUse.length)(_.d(2))
    val preAdded = if (symmetric) {
      val xlinePost = x.d(coeffsInUse.length * 2)
      xline.map(x => (x +^ xlinePost.d()).d())
    } else xline

    val sintCoeffs = coeffsInUse.map(coeffType.fromConstant).map(_.asSInt())
    val scaled = preAdded.zip(sintCoeffs).map { case (port, coeff) =>
      (port * coeff.d()).d()
    }
    val zero = S(0, productType.bitWidth bits)
    // the first element is a dummy, it is a must for extreme fmax, or PREG won't be used for the first DSP
    val ret = (zero +: scaled).reduce((a, b) =>
      (a + b).d()
    ) // addition without width growth

    // SInt datapath end
    dataOut.head.assignFromBits(ret.d().asBits)
    lastOut := lastIn.validAfter(latency())
  }
}
