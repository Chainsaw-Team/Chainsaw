package Chainsaw.dsp

/** Spectrum shifting of the input signal by multiplying with a carrier
  */

import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

case class ShiftSpec(
    inputFreq: HertzNumber,
    carrierFreq: HertzNumber,
    dataType: NumericType,
    complex: Boolean,
    parallel: Int
) extends ChainsawInfiniteGenerator
    with FixedLatency {

  override def name: String = "ShiftSpec"

  override def inputTypes: Seq[NumericType] = Seq.fill(parallel)(dataType)

  override def outputTypes: Seq[NumericType] = Seq.fill(if (complex) 2 * parallel else parallel)(dataType)

  val ddsGen = Dds(
    ddsWave = DdsWave(
      signalType   = SINE,
      samplingFreq = inputFreq,
      signalFreq   = carrierFreq,
      amplitude    = 1.0,
      phaseOffset  = 0.0,
      complex      = complex
    ),
    dataType = NumericType.SFix(1, 14),
    parallel = parallel
  )
  override def implH: ChainsawInfiniteModule = new ChainsawInfiniteModule(this) {

    val ddsFlow = flowIn.mapFragment(_ => Seq[AFix]()) >> ddsGen
    val delayedData = dataIn.d(ddsGen.latency())

    val shiftedFlow: ChainsawFlow =
      if (complex)
        ddsFlow.mapFragment(
          func = _.toComplexFix
            .zip(delayedData)
            .map { case (sine, data) => (sine * data).d(2).fixTo(dataType) }
            .toAFixVec,
          latency = 2
        )
      else
        ddsFlow.mapFragment(
          func    = _.zip(delayedData).map { case (sine, data) => (sine * data).d(2).fixTo(dataType()) },
          latency = 2
        )

    shiftedFlow >> flowOut
  }

  override def implNaiveH: Option[ChainsawInfiniteModule] = None

  override def latency(): Int = 4

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil()

  override def fmaxEstimation: HertzNumber = 800 MHz

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = {
    val carrier: Seq[Double] = DdsWave(SINE, inputFreq, carrierFreq, 1.0, 0.0, complex)
      .generate(testCase.data.length)
    if (complex) {
      val cos  = carrier.grouped(2).map(_.head).toSeq
      val sin  = carrier.grouped(2).map(_.last).toSeq
      val real = cos.zip(testCase.data).map { case (c, d) => c * d }
      val imag = sin.zip(testCase.data).map { case (s, d) => s * d }
      real.zip(imag).flatMap { case (r, i) => Seq(r, i) }
    } else carrier.zip(testCase.data).map { case (c, d) => c * d }
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = corrMetric(yours, golden, 0.9)

  override def testCases: Seq[TestCase] = Seq.fill(10)(randomTestCase(1000))

  override def resetCycle: Int = 1
}
