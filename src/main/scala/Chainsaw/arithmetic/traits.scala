package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx.{VivadoUtil, VivadoUtilEstimation}
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
 *
 */
trait UnsignedMultiplier extends ChainsawOperatorGenerator with MultAttribute {

  override def inputTypes =
    if (isConstantMult || multiplierType == SquareMultiplier) Seq(NumericType.U(widthX))
    else Seq(widthX, widthY).map(NumericType.U)

  override def outputTypes = Seq(NumericType.U(widthOut))

  override def impl(testCase: TestCase) = {
    // caution! big error will be introduced when using BigDecimal directly
    val temp =
      if (isConstantMult) constant.get * testCase.data.head.toBigInt()
      else if (multiplierType == SquareMultiplier) testCase.data.head.toBigInt() * testCase.data.head.toBigInt()
      else testCase.data.map(_.toBigInt()).product
    val ret = multiplierType match {
      case MsbMultiplier => temp >> (widthX + widthY - widthOut)
      case LsbMultiplier => temp.mod(Pow2(widthOut))
      case _ => temp
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
          case LsbMultiplier => raw.takeLow(widthOut).asUInt
          case MsbMultiplier => raw.takeHigh(widthOut).asUInt
          case FullMultiplier => raw
          case SquareMultiplier => raw
        }
      }.toAFix.d(latency())
    })

  override def testCases = Seq.fill(10000)(TestCase(randomDataVector))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    multiplierType match {
      case MsbMultiplier =>
        val error = golden.head - yours.head
        error.abs < (widthOut / 2)
      case _ => yours.equals(golden)
    }
  }
}

trait UnsignedMerge extends ChainsawOperatorGenerator with Unaligned {

  def arithInfos: Seq[ArithInfo]

  val timeMin = arithInfos.map(_.time).min
  override def inputTimes = arithInfos.map(_.time - timeMin)

  override def outputTimes = Seq(0)

  override def inputTypes = arithInfos.map(_.width).map(NumericType.U)

  def compensation = {
    val negatives = arithInfos.filterNot(_.isPositive).map(info => ((BigInt(1) << info.width) - 1) << info.weight)
    (negatives :+ BigInt(0)).sum // in case of empty
  }

  def maxValue = arithInfos.map(_.toPositive.maxValue).sum - compensation
  require(maxValue == arithInfos.map(_.maxValue).sum)

  def validLength = maxValue.bitLength

  override def impl(testCase: TestCase) = {
    val temp = testCase.data.map(_.toBigInt()).zip(arithInfos).map { case (data, info) => info.eval(data) }.sum
    Seq(BigDecimal(temp)).padTo(outputTypes.length, BigDecimal(0))
  }

  override def implNaiveH =
    Some(new ChainsawOperatorModule(this) {
      val ops = dataIn.zip(inputTimes).map { case (op, time) => op.d(inputInterval - time) }
      val opAndInfos = ops.map(_.asUInt()).zip(arithInfos)
      val positive = opAndInfos.filter(_._2.isPositive).map { case (int, info) => int << info.weight }.reduce(_ +^ _)
      val negative =
        if (arithInfos.exists(!_.isPositive)) opAndInfos.filterNot(_._2.isPositive).map { case (int, info) => int << info.weight }.reduce(_ +^ _)
        else U(0)
      val ret = (positive - negative).d(latency() - inputInterval)
      dataOut.head := ret.toAFix.truncated
      dataOut.tail.foreach(port => port := port.getZero)
    })

  override def testCases = Seq.fill(100)(TestCase(randomDataVector))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    if (golden.exists(_ < 0)) {
      logger.info("negative sum occur in your testcase")
      true
    } // skip
    else {
      val yourSum = yours.map(_.toBigInt()).sum
      val goldenSum = golden.map(_.toBigInt()).sum
      yourSum.mod(Pow2(validLength)) == goldenSum.mod(Pow2(validLength))
    }
  }
}