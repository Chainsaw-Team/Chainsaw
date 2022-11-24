package Chainsaw.dsp

import Chainsaw._
import breeze.numerics.constants.Pi
import breeze.numerics.sin
import spinal.core._
import spinal.lib._
import ChainsawMetric._

sealed trait DdsSignalType

object SINE extends DdsSignalType

object PULSE extends DdsSignalType


case class DdsWave(signalType: DdsSignalType,
                   baseBandFreq: HertzNumber,
                   signalFreq: HertzNumber,
                   phaseOffset: Double) {

  val baseValue = baseBandFreq.toBigDecimal.toBigInt()
  val signalValue = signalFreq.toBigDecimal.toBigInt()

  val commonFreq = lcm(baseValue, signalValue)

  val pointsInCommonPeriod: Int = (commonFreq / signalValue).toInt
  logger.info(s"dds period: $pointsInCommonPeriod")
  val pointsInSignalPeriod: Double = baseBandFreq.toDouble / signalFreq.toDouble

  def wave = signalType match {
    case SINE =>
      val step = 2 * Pi / pointsInSignalPeriod
      (0 until pointsInCommonPeriod).map(i => sin(i * step + phaseOffset))
    case PULSE => ???
  }

  def generate(cycle: Int) = {
    val periodCount = cycle.divideAndCeil(pointsInCommonPeriod)
    Seq.fill(periodCount)(wave).flatten.take(cycle)
  }
}

/** generating periodic wave
 *
 * @param dataType output data precision
 */
case class Dds(ddsWave: DdsWave, dataType: NumericType) extends ChainsawGenerator {

  override def name = "dds"

  override def impl(dataIn: Seq[Any]) = ddsWave.generate(dataIn.length)

  override val implMode = Infinite

  override val metric = ChainsawMetric(frameWise = forallBound(doubleBound(1e-3)))

  override def inputTypes = Seq(UIntInfo(1))
  override def outputTypes = Seq(dataType)

  override def inputFormat = inputNoControl
 override def outputFormat = outputNoControl
  override def latency = 2

  override def implH = new ChainsawModule(this) {
    val data = ddsWave.wave.map(dataType.fromConstant)
    val signalRom = Mem(data)
    val counter = Counter(ddsWave.pointsInCommonPeriod, inc = validIn)
    sfixDataOut.head := signalRom.readSync(counter.value).d()
  }
}
