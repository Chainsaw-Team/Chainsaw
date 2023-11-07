package Chainsaw.dsp

import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.edaFlow.vivado._
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

  // size suitable for both Xilinx and Intel DSP blocks
  val scalingFactorType = NumericType.SFix(0, 24)

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
  val adderTreeLatency    = log2Up(size)
  val scalingLatency      = 2
  override def latency()  = adderTreeLatency + scalingLatency + 4 // 4 for control compensations
  override def resetCycle = size

  override def controlTypes = Seq(NumericType.U(log2Up(size + 1)))

  override def implH = new ChainsawDynamicInfiniteModule(this) {

    // control -> controlInUse, latency = 1
    val control      = controlIn.head.asUInt()
    val controlInUse = RegNextWhen(control, validIn) // lock the control until next validIn
    // multi-hot decoder implemented by ROM, latency = 2
    val decoderSize  = nextPow2(size + 1).toInt
    val decoderValue = (0 until decoderSize).map(i => if (i >= size) B("1" * size) else B("1" * i + "0" * (size - i)))
    val decoderRom   = Mem(decoderValue)
    val valids       = decoderRom.readSync(controlInUse).d() // control -> valids, latency = 3
    // scaling factor ROM
    val scalingFactors   = Seq(2.0, 2.0) ++ (2 to size).map(_.toDouble)
    val scalingFactorRom = Mem(scalingFactors.map(i => scalingFactorType.fromConstant(1 / i)))
    val scalingFactor    = scalingFactorRom.readSync(controlInUse) // control -> scalingFactor, latency = 3
    // align dataIn with controlIn
    val data = Mux(validIn, dataIn.head, dataIn.head.getZero).d(3)

    // build delay line
    val history = Seq.iterate(data, size)(_.d())
    // get sum
    val validElements = history.zip(valids.asBools.reverse).map { case (ele, bool) => Mux(bool, ele, ele.getZero).d() }
    val sum: AFix = validElements
      .pipelinedBalancedTree(_ + _, 1)
      .d()
      .fixTo(dataType() << log2Up(size)) // !! fixTo here is not a cost-free operation!
    // get mean
    dataOut.head := (sum mult scalingFactor).d(2).fixTo(dataType(), roundType = RoundType.FLOOR)

    lastOut := lastIn.validAfter(latency())
  }
}
