package Chainsaw.dsp

import Chainsaw.NumericExt.SFixUtil
import Chainsaw._
import Chainsaw.xilinx.VivadoUtilRequirement
import spinal.core._

import scala.util.Random

/** numeric types of input, coeffs and output
 */
case class FilterPrecision(
                            coeffType: NumericType,
                            dataType: NumericType
                          ) {

  val retType = dataType * coeffType

  override def toString = s"${coeffType.integral}_${coeffType.fractional}_${dataType.integral}_${dataType.fractional}"
}

/** systolic fir for FPGAs, extremely efficient for Xilinx device
 */
case class Fir(coeffs: Seq[Double], filterPrecision: FilterPrecision, symmetric: Boolean = false)
  extends ChainsawGenerator {

  import filterPrecision._

  if (symmetric) require(coeffs.length % 2 == 0, "odd tap number is not supported for symmetric mode")

  val productType = SFixInfo(
    dataType.integral + coeffType.integral + 1 + (if (symmetric) 1 else 0),
    dataType.fractional + coeffType.fractional)

  override def name = getAutoName(this)

  override def impl(dataIn: Seq[Any]) =
    fir(dataIn.asInstanceOf[Seq[Double]], coeffs).drop(coeffs.length - 1)

  override val implMode = Infinite

  override val metric = ChainsawMetric(frameWise = corrMetric) // magnitude-irrelevant metric

  override def generateTestCases: Seq[Double] = Seq.fill(1000)(Random.nextDouble())

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(productType)

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl

  val coeffsInUse = if (symmetric) coeffs.take(coeffs.length.divideAndCeil(2)) else coeffs

  override def latency = if (symmetric) 3 * (coeffsInUse.length + 1) + 1 else 2 * (coeffsInUse.length + 1) + 1

  override def utilEstimation = VivadoUtilRequirement(dsp = coeffsInUse.length)

  logger.info(s"dsp estimation = ${utilEstimation.dsp} for $name")

  // FIXME: fmaxEstimation lead to a bug in getAutoName
  //  override def fmaxEstimation = 600 MHz

  override def implH: ChainsawModule = new ChainsawModule(this) {
    val x = sfixDataIn.head
    val xline = Seq.iterate(x.d(2), coeffsInUse.length)(_.d(2))
    val preAdded = if (symmetric) {
      val xlinePost = x.d(coeffsInUse.length * 2)
      xline.map(x => (x +^ xlinePost.d()).d())
    } else xline
    val scaled = preAdded.zip(coeffsInUse).map { case (port, coeff) => (port * coeffType.fromConstant(coeff).d()).d() }
    val zero = productType.fromConstant(0.0)
    // the first element is a dummy, it is a must for extreme fmax, or PREG won't be used for the first DSP
    val ret = (zero +: scaled).reduce((a, b) => (a + b).d())
    sfixDataOut.head := ret.d()

  }
}
