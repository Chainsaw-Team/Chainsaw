package Chainsaw.dsp

import Chainsaw.{DdsSignalType, SINE, lcm}
import breeze.numerics.constants.Pi
import breeze.numerics.{cos, sin}
import spinal.core.{HertzNumber, IntToBuilder}
import Chainsaw._
import Chainsaw.xilinx._

import scala.language.postfixOps

case class DdsWave(signalType: DdsSignalType,
                   samplingFreq: HertzNumber,
                   signalFreq: HertzNumber,
                   amplitude: Double,
                   phaseOffset: Double,
                   complex: Boolean = false) {

  val baseValue = samplingFreq.toBigDecimal.toBigInt()
  val signalValue = signalFreq.toBigDecimal.toBigInt()

  val commonFreq = lcm(baseValue, signalValue)

  val pointsInCommonPeriod: Int = (commonFreq / signalValue).toInt
  val period = pointsInCommonPeriod

  val pointsInSignalPeriod: Double = samplingFreq.toDouble / signalFreq.toDouble

  def wave = signalType match {
    case SINE =>
      val step = 2 * Pi / pointsInSignalPeriod
      val points = 1 to pointsInCommonPeriod
      val sineWave = points.map(i => sin(i * step + phaseOffset) * amplitude)
      val cosWave = points.map(i => cos(i * step + phaseOffset) * amplitude)
      if (complex) cosWave.zip(sineWave).flatMap { case (cos, sin) => Seq(cos, sin) }
      else sineWave
  }

  def generate(points: Int) = {
    val periodCount = points.divideAndCeil(pointsInCommonPeriod)
    Seq.fill(periodCount)(wave).flatten.take(points * (if (complex) 2 else 1))
  }

  override def toString = s"${className(signalType)}_" +
    s"${(samplingFreq / 1e6).toInt}_${(signalFreq / 1e6).toInt}_" +
    s"${amplitude.toInt}_${(phaseOffset / Pi * 180).toInt}_${if (complex) "C" else "R"}"
}

object DdsWave extends App {
  val wave = DdsWave(SINE, 100 MHz, 1 MHz, 1, 0)
  println(s"wave.toString = ${wave.toString}")
  println(s"wave.generate(200) = ${wave.generate(200).mkString("\n")}")
}
