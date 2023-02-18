package Chainsaw.dsp

import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

// TODO: parallel version
case class DynamicMovingAverage(size: Int, dataType: NumericType)
    extends ChainsawDynamicInfiniteGenerator
    with FixedLatency {

  override def implNaiveH = None

  override def name = s"DynamicMovingAverage_${size}_$dataType"

  override def vivadoUtilEstimation =
    VivadoUtil(lut = (dataType.bitWidth * size * 1.2).toInt, dsp = 1)

  override def fmaxEstimation = 600 MHz

  override def inputTypes = Seq(dataType)

  override def outputTypes = Seq(dataType)

  override def impl(testCase: TestCase) = {
    val size   = testCase.control.head.toInt
    val padded = Seq.fill(size - 1)(BigDecimal(0)) ++ testCase.data
    padded.sliding(size).map(_.sum / size).toSeq
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    require(yours.length == golden.length)
    corrMetric(yours, golden, 0.9)
  }

  override def testCases = {
    val sizes = Seq.fill(3)(Random.nextInt(size) + 3 min size)
    sizes.map(size => TestCase(randomDataSequence(Random.nextInt(size * 3) + 5), Seq(size)))
  }

  val adderTreeLatency = log2Up(size)
  val scalingLatency   = 2

  override def latency() =
    adderTreeLatency + scalingLatency + 4 // 4 for control compensations

  override def resetCycle = size

  override def controlTypes = Seq(NumericType.U(log2Up(size + 1)))

  override def implH = new ChainsawDynamicInfiniteModule(this) {

    // controlIn -> valids, latency = 3
    val control = controlIn.head.asUInt()
    // TODO: build a method: lock
    val controlInUse =
      RegNextWhen(control, validIn) // lock the control until next validIn
    // this decoder is a bottleneck when size is large, considering implement it by RAM

    val decoderValue = (0 until nextPow2(size + 1).toInt)
      .map(i => if (i >= size) B("1" * size) else B("1" * i + "0" * (size - i)))
    val decoderRom = Mem(decoderValue)
    val valids     = decoderRom.readSync(controlInUse).d()

    // delay line
    // dataIn -> head, latency = 3
    val head    = Mux(validIn, dataIn.head, dataIn.head.getZero).d(3)
    val history = Seq.iterate(head, size)(_.d())
    // get moving sum
    val validElements = history.zip(valids.asBools.reverse).map { case (ele, bool) =>
      Mux(bool, ele, ele.getZero).d()
    }
    val sum: AFix = validElements
      .pipelinedBalancedTree(_ + _, 1)
      .fixTo(dataType() << log2Up(size))

    // get mean
    val scalingFactorType = NumericType(
      integral   = 0,
      fractional = 25,
      signed     = true
    ) // size suitable for both Xilinx and Intel DSP blocks
    // TODO: build a method: ROM by
    val scalingFactor = {
      val scalingFactorRom = Mem(
        (Seq(2.0, 2.0) ++ (2 to size).map(_.toDouble)).map(i => scalingFactorType.fromConstant(1 / i))
      )
      scalingFactorRom.readSync(controlInUse)
    }
    dataOut.head := (sum * scalingFactor).d(2).fixTo(dataType(), roundType = RoundType.FLOOR)
    lastOut      := lastIn.validAfter(latency())
  }
}
