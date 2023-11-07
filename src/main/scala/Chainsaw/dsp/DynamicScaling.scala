package Chainsaw.dsp

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._
import spinal.lib.fsm._
import spinal.lib.bus._
import Chainsaw._
import Chainsaw.edaFlow.vivado._

import scala.collection.mutable.ArrayBuffer

object Normalization {

  def normalize(data: Seq[Double], period: Int, overflowLimit: Double, underflowLimit: Double) = {
    require(data.length % period == 0)
    var scalingFactor       = 1.0
    var i                   = 0
    var overflow, underflow = 0.0
    val ret                 = ArrayBuffer[Double]()

    while (i < data.length) {
      for (elem <- (0 until period)) {
        val prod = data(i) * scalingFactor
        ret += prod
        if (prod.abs > 1) overflow += 1
        if (prod.abs < 0.5) underflow += 1
        i += 1
      }
      if (overflow / period > overflowLimit) scalingFactor -= 0.01
      else if (underflow / period > underflowLimit) scalingFactor += 0.01
      overflow  = 0
      underflow = 0
    }
    println(s"final scaling factor = $scalingFactor")
    ret
  }

  def main(args: Array[String]): Unit = {
    val period = 1024
    val data   = Seq.fill(period * 100)(scala.util.Random.nextDouble() * 4)
    normalize(data, period, 0.1, 0.9)
  }
}

/** */
case class DynamicScaling() extends ChainsawOperatorGenerator {
  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {}

  override def implNaiveH: Option[ChainsawOperatorModule] = ???

  override def name: String = ???

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation: VivadoUtil = ???

  override def fmaxEstimation: HertzNumber = ???

  /** -------- interfaces
    * --------
    */
  override def inputTypes: Seq[NumericType] = ???

  override def outputTypes: Seq[NumericType] = ???

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = ???

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = ???

  override def testCases: Seq[TestCase] = ???
}
