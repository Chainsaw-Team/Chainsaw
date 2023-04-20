package Chainsaw.dsp

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._
import spinal.lib.fsm._
import spinal.lib.bus._
import Chainsaw._
import Chainsaw.xilinx._

import scala.language.postfixOps

case class UpFirDn(
    freqData: HertzNumber,
    freqClk: HertzNumber,
    coeffs: Seq[Double],
    factorUp: Int,
    factorDown: Int,
    coeffType: NumericType,
    dataType: NumericType
) extends ChainsawInfiniteGenerator
    with FixedLatency {

  // parameters validation
  val parallelFactor = (freqData / freqClk).toBigIntExact() match {
    case Some(value) => value
    case None        => throw new IllegalArgumentException("freqData and freqClk are not integer multiples")
  }

  logger.info(s"parallelFactor is $parallelFactor")
  override def implH: ChainsawInfiniteModule = ???

  override def implNaiveH: Option[ChainsawInfiniteModule] = ???

  override def latency(): Int = ???

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

  override def resetCycle: Int = ???
}

object UpFirDn {
  def main(args: Array[String]): Unit = {
    val upFirDn = UpFirDn(
      freqData   = 250 MHz,
      freqClk    = 125 MHz,
      coeffs     = Seq(1, 2, 3),
      factorUp   = 2,
      factorDown = 2,
      coeffType  = NumericType.SFix(0, 14),
      dataType   = NumericType.SFix(0, 14)
    )

  }
}
