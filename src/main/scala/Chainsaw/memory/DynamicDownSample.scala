package Chainsaw.memory

import Chainsaw.xilinx.VivadoUtil
import Chainsaw._
import spinal.core._

import scala.util.Random     // for Xilinx FPGA Flow

case class DynamicDownSample(factor: Int, dataType: NumericType) extends ChainsawDynamicInfiniteGenerator {
  override def implNaiveH = None

  override def controlTypes = Seq(NumericType.U(log2Up(factor)))

  override def name = s"DynamicDownSample_${factor}"

  override def vivadoUtilEstimation = VivadoUtil()

  override def fmaxEstimation = 600 MHz

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(dataType)

  override def impl(testCase: TestCase) = {
    val factor = testCase.control.head.toInt
    logger.info(s"factor is $factor")
    logger.info(s"testCase size is ${testCase.data.length}")
    testCase.data.grouped(factor).map(_.last).toSeq
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    ChainsawMetric.sameAsBigInt(yours, golden)

  override def testCases = Seq.fill(10)(randomTestCase(Random.nextInt(100) + 1))

  override def resetCycle = 0

  override def implH =
    new ChainsawDynamicInfiniteModule(this) {
      val currentFactor = controlIn.head.asUInt()
      val counter       = DynamicCounter(currentFactor)
      when(lastIn)(counter.clear())
      when(validIn)(counter.increment())
      dataOut  := dataIn.d()
      validOut := (counter.willOverflow && validIn || lastIn).validAfter(1) // the last one is a must
      lastOut  := lastIn.validAfter(1)
    }
}
