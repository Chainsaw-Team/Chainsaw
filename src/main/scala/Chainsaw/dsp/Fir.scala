package Chainsaw.dsp

import Chainsaw._
import Chainsaw.edaFlow.vivado.VivadoUtil
import Chainsaw.io.pythonIo.runPythonModel
import spinal.core._

import scala.language.postfixOps

/** systolic fir for FPGAs, extremely efficient for Xilinx device
  */
case class Fir(
    b: Seq[Double],
    coeffType: NumericType,
    dataType: NumericType
) extends ChainsawInfiniteGenerator
    with FixedLatency {

  val tap       = b.length
  val half      = tap / 2
  val symmetric = b.take(half).zip(b.takeRight(half).reverse).forall { case (a, b) => ((a - b).abs / a.abs) < 1e-4 }
  if (symmetric) println("your filter is a symmetric FIR")
  val temp        = dataType * coeffType
  val productType = if (symmetric) NumericType(temp.integral + 1, temp.fractional, temp.signed) else temp
  val coeffsInUse = if (symmetric) {
    if (tap % 2 == 0) b.take(half)
    else b.take(half) :+ (b(half) / 2)
  } else b
//  val outputType = productType.withCarry(log2Up(tap))

  override def name = s"${if (symmetric) "Symmetric"
  else "Asymmetric"}_Fir_${dataType}_${coeffType}_coeff${hashName(b)}"

  override def implNaiveH = None

  override def impl(testCase: TestCase) =
    runPythonModel("dsp", "lfilter", testCase.data, Some(testCase.control), Some(this))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    corrMetric(yours, golden, 0.9)

  override def testCases = Seq.fill(3)(randomTestCase(1000))

  override def resetCycle = latency()

  override def latency() =
    if (symmetric) coeffsInUse.length + 6
    else coeffsInUse.length + 5

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(productType)

  override def vivadoUtilEstimation = VivadoUtil(dsp = coeffsInUse.length)

  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawInfiniteModule(this) {

    val data = dataIn.head.asSInt()
    val x    = Mux(validIn, data, data.getZero).d()

    // using SInt inside for better inference in Quartus, as AFix multiplication do sign extension, which won't be recognized and optimized by Quartus
    // SInt datapath
    val xline = Seq.iterate(x.d(2), coeffsInUse.length)(_.d(2))
    val preAdded = if (symmetric) {
      val xlinePost = x.d(tap)
      xline.map(x => (x +^ xlinePost.d()).d())
    } else xline

    val sintCoeffs = coeffsInUse.map(coeffType.fromConstant).map(_.asSInt())
    val scaled     = preAdded.zip(sintCoeffs).map { case (port, coeff) => (port * coeff.d()).d() }
    val zero       = S(0, productType.bitWidth bits)
    // the first element is a dummy, it is a must for extreme fmax, or PREG won't be used for the first DSP
    val ret = (zero +: scaled).reduce((a, b) => (a + b).d()) // addition without width growth

    // SInt datapath end
    dataOut.head.assignFromBits(ret.d().asBits)
    lastOut := lastIn.validAfter(latency())
  }
}
