package Chainsaw.dsp

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import Chainsaw.NumericExt._

import scala.language.postfixOps // for Xilinx FPGA Flow

/** unwrap for normalized data(Pi -> 1)
  * @param numericType
  *   unified input/output type for unwrap
  */
case class UnwrapPointByPoint(numericType: NumericType)
    extends ChainsawOperatorGenerator
    with FixedLatency {

  override def name: String = s"unwrap_${numericType}"

  override def inputTypes: Seq[NumericType] = Seq.fill(2)(numericType)

  override def outputTypes: Seq[NumericType] = Seq(numericType)

  override def impl(testCase: TestCase): Seq[BigDecimal] = {
    val Seq(prev, next) = testCase.data
    val diff            = (next - prev).toDouble
    val roundedDiff     = scala.math.round(diff / scala.math.Pi / 2)
    val ret             = next - roundedDiff * scala.math.Pi * 2
    Seq(ret)
  }

  override def metric(
      yours: Seq[BigDecimal],
      golden: Seq[BigDecimal]
  ): Boolean = yours.zip(golden).forall { case (y, g) => (y - g).abs <= 1e-1 }

  override def testCases: Seq[TestCase] =
    Seq.fill(1000)(TestCase(randomDataVector.map(_ * 0.5)))
  override def implH = new ChainsawOperatorModule(
    this
  ) {

    val piReciprocal =
      NumericType.SFix(0, 17).fromConstant(1 / (scala.math.Pi * 2))
    val pi = NumericType.SFix(4, 13).fromConstant(scala.math.Pi * 2)

    val Seq(prev, next) = dataIn

    val diff        = (next - prev).d()
    val scaledDiff  = (diff * piReciprocal).d(2)
    val roundedDiff = scaledDiff.roundWithPipeline.toAFix
    val multiple    = (roundedDiff * pi).d(2)

    dataOut.head := (next.d(7) -| multiple).d().truncated
  }

  override def implNaiveH: Option[ChainsawOperatorModule] = ???

  override def vivadoUtilEstimation: VivadoUtil = VivadoUtil(dsp = 2)

  override def fmaxEstimation: HertzNumber = 600 MHz

  override def resetCycle: Int = 0

  override def latency(): Int = 8
}
