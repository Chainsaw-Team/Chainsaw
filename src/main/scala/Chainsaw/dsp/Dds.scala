package Chainsaw.dsp

import Chainsaw._
import Chainsaw.xilinx.VivadoUtil
import spinal.core._
import spinal.lib._
import scala.util.Random

import scala.language.postfixOps

/** generating periodic wave
  *
  * @param dataType
  *   output data precision
  */
case class Dds(ddsWave: DdsWave, dataType: NumericType, parallel: Int)
    extends ChainsawInfiniteGenerator
    with FixedLatency {

  def name = s"Parallel_${parallel}_${Dds}_${dataType}_$ddsWave"

  logger.info(s"wave period = ${ddsWave.pointsInCommonPeriod}")
  val actualPeriod = lcm(ddsWave.period, parallel).toInt

  override def implNaiveH = None

  override def impl(testCase: TestCase) =
    ddsWave.generate(testCase.data.length * parallel).map(BigDecimal(_))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    corrMetric(yours, golden, 0.9)

  override def testCases =
    Seq.fill(3)(TestCase(randomDataSequence(Random.nextInt(100) + 10)))

  override def resetCycle = 0

  override def latency() = 2

  override def inputTypes = Seq[NumericType]()

  override def outputTypes =
    Seq.fill(parallel * (if (ddsWave.complex) 2 else 1))(dataType)

  override def vivadoUtilEstimation = VivadoUtil()

  override def fmaxEstimation = 600 MHz

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
}
