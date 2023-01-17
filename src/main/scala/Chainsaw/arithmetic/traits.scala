package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx.{VivadoUtil}
import breeze.numerics.ceil
import spinal.core._

import scala.language.postfixOps

trait MultAttribute {
  def constant: Option[BigInt]

  def widthX: Int

  def widthY: Int

  def multiplierType: MultiplierType

  def widthOut: Int

  def isConstantMult = constant.isDefined

  def vivadoUtilEstimation: VivadoUtil

  def clbCost = vivadoUtilEstimation.lut

  def dspCost = vivadoUtilEstimation.dsp

  def report(methodName: String) = {
    s"$methodName Solution for $widthX X $widthY bit ${className(multiplierType)}: " +
      s"\n\tdspCost = $dspCost, clbCost = $clbCost"
  }
}

/** unified behavioral model of unsigned multiplier
  */
trait UnsignedMultiplier
    extends ChainsawOperatorGenerator
    with MultAttribute
    with FixedLatency {

  override def inputTypes =
    if (isConstantMult || multiplierType == SquareMultiplier)
      Seq(NumericType.U(widthX))
    else Seq(widthX, widthY).map(NumericType.U)

  override def outputTypes = Seq(NumericType.U(widthOut))

  override def impl(testCase: TestCase) = {
    // caution! big error will be introduced when using BigDecimal directly
    val temp =
      if (isConstantMult) constant.get * testCase.data.head.toBigInt()
      else if (multiplierType == SquareMultiplier)
        testCase.data.head.toBigInt() * testCase.data.head.toBigInt()
      else testCase.data.map(_.toBigInt()).product
    val ret = multiplierType match {
      case MsbMultiplier => temp >> (widthX + widthY - widthOut)
      case LsbMultiplier => temp.mod(pow2(widthOut))
      case _             => temp
    }
    Seq(BigDecimal(ret))
  }

  override def implNaiveH =
    Some(new ChainsawOperatorModule(this) {

      val y =
        if (isConstantMult) U(constant.get, constant.get.bitLength bits)
        else dataIn.last.asUInt()
      val raw = dataIn.head.asUInt() * y

      dataOut.head := {
        multiplierType match {
          case LsbMultiplier    => raw.takeLow(widthOut).asUInt
          case MsbMultiplier    => raw.takeHigh(widthOut).asUInt
          case FullMultiplier   => raw
          case SquareMultiplier => raw
        }
      }.toAFix.d(latency())
    })

  override def testCases = Seq.fill(10000)(TestCase(randomDataVector))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    val diff = golden.head.toBigInt() - yours.head.toBigInt()
    if (diff != BigInt(0)) logger.info(s"diff = $diff")
    multiplierType match {
      case MsbMultiplier =>
        diff.abs < (widthOut / 2)
      case _ => diff == 0
    }
  }
}
