package Chainsaw.dsp
import Chainsaw._
import Chainsaw.io.pythonIo._
import Chainsaw.xilinx._
import spinal.core._

import java.io.File
import scala.language.postfixOps

case class PeriodicUnwrap(numericType: NumericType, periodMax: Int)
  extends ChainsawDynamicInfiniteGenerator {

  override def name: String = s"PeriodicUnwrap_${numericType}_$periodMax"
  override def inputTypes: Seq[NumericType] = Seq(numericType)

  override def outputTypes: Seq[NumericType] = Seq(numericType)
  override def controlTypes: Seq[NumericType] = Seq(
    NumericType.U(log2Up(periodMax + 1)), // period
    NumericType.Bool()                    // reset
  )

  val delayGen  = DynamicDelay(periodMax, numericType, 1)
  val unwrapGen = UnwrapPointByPoint(numericType)
  override def implH: ChainsawDynamicInfiniteModule =
    new ChainsawDynamicInfiniteModule(this) {

      val period = controlIn.head.asUInt()
      val reset  = controlIn.last.asUInt().asBool

      val delay  = delayGen.implH
      val unwrap = unwrapGen.implH

      // feed unwrap operator
      val prev = Mux(delay.validOut && !reset, delay.dataOut.head, dataIn.head)
      val next = dataIn.head
      ChainsawFlow(Vec(prev, next), validIn, lastIn) >> unwrap.flowIn

      // feed delay line
      delay.controlIn.head := period.toAFix -| U(unwrapGen.latency()).toAFix
      ChainsawFlow(unwrap.dataOut, unwrap.validOut, lastIn) >> delay.flowIn

      unwrap.flowOut >> flowOut
    }

  override def implNaiveH: Option[ChainsawDynamicInfiniteModule] = ???

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil()
  override def fmaxEstimation: HertzNumber      = 600 MHz
  override def impl(testCase: TestCase): Seq[BigDecimal] =
    goldenModelBySignal(
      new File(pythongProjectDir, "utils/periodic_unwrap.py"),
      testCase.control.head.toInt.toString,
      testCase.data
    ).head

  override def metric(
                       yours: Seq[BigDecimal],
                       golden: Seq[BigDecimal]
                     ): Boolean = corrMetric(yours, golden, threshold = 0.9)

  override def testCases: Seq[TestCase] =
    Seq.fill(10)(
      TestCase(randomDataSequence(periodMax * 10).map(_ * 0.01), Seq(periodMax, 0))
    )
  override def resetCycle: Int = periodMax
}
