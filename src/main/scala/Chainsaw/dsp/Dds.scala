package Chainsaw.dsp

import Chainsaw._
import Chainsaw.xilinx.VivadoUtil
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

/** generating periodic wave
  * @param ddsWave
  *   a DdsWave object which defines the waveform
  * @param dataType
  *   numeric type of output data
  */
case class Dds(ddsWave: DdsWave, dataType: NumericType, parallel: Int)
    extends ChainsawInfiniteGenerator
    with FixedLatency {

  def name = s"${Dds}_${dataType}_${ddsWave}_Parallel_${parallel}"

  override def impl(testCase: TestCase) =
    ddsWave.generate(testCase.data.length * parallel).map(BigDecimal(_))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    corrMetric(yours, golden, 0.9)

  override def testCases =
    Seq.fill(3)(TestCase(randomDataSequence(Random.nextInt(100) + 10)))

  override def resetCycle = 0

  override def latency() = 2

  override def inputTypes = Seq[NumericType]()

  override def outputTypes = Seq.fill(parallel * (if (ddsWave.complex) 2 else 1))(dataType)

  override def vivadoUtilEstimation = VivadoUtil()

  override def fmaxEstimation = 600 MHz

  val actualPeriod = lcm(ddsWave.period, parallel).toInt
  override def implH = new ChainsawInfiniteModule(this) {
    val data = ddsWave
      .generate(actualPeriod * parallel)
      .grouped(outPortWidth)
      .toSeq
      .map(seq => Vec(seq.map(dataType.fromConstant)))
    val signalRom = Mem(data)
    val counter   = Counter(actualPeriod, inc = validIn)
    when(!validIn)(counter.clear())
    dataOut := signalRom.readSync(counter.value).d()
    lastOut := lastIn.d(latency())
  }

  override def implNaiveH = None
}

import Chainsaw.{DdsSignalType, SINE, lcm}
import breeze.numerics.constants.Pi
import breeze.numerics.{cos, sin}
import spinal.core.HertzNumber

import scala.language.postfixOps

/** definition of a DDSWave
  */
case class DdsWave(
    signalType: DdsSignalType,
    samplingFreq: HertzNumber,
    signalFreq: HertzNumber,
    amplitude: Double,
    phaseOffset: Double,
    complex: Boolean = false
) {

  val baseValue   = samplingFreq.toBigDecimal.toBigIntExact().get
  val signalValue = signalFreq.toBigDecimal.toBigIntExact().get

  val pointsInSignalPeriod = samplingFreq / signalFreq

  val commonFreq           = lcm(baseValue, signalValue)
  val pointsInCommonPeriod = (commonFreq / signalValue)
  require(pointsInCommonPeriod <= 65536, "way too big common period")
  val period = pointsInCommonPeriod.toInt

  /** get a period of the periodic waveform
    */
  def wave = signalType match {
    case SINE =>
      val step     = 2 * Pi / pointsInSignalPeriod.toDouble
      val points   = 0 until period
      val sineWave = points.map(i => sin(i * step + phaseOffset) * amplitude)
      val cosWave  = points.map(i => cos(i * step + phaseOffset) * amplitude)
      if (complex) cosWave.zip(sineWave).flatMap { case (cos, sin) => Seq(cos, sin) }
      else sineWave
  }

  /** get waveform with a specified number of points
    */
  def generate(points: Int) = {
    val periodCount = points.divideAndCeil(period)
    Seq.fill(periodCount)(wave).flatten.take(points * (if (complex) 2 else 1))
  }

  override def toString = s"${className(signalType)}_" +
    s"${(samplingFreq / 1e6).toInt}_${(signalFreq / 1e6).toInt}_" +
    s"${amplitude.toInt}_${(phaseOffset / Pi * 180).toInt}_${if (complex) "C" else "R"}"
}
