package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

case class BmNew(bmSolution: BmSolution) extends ChainsawOperatorGenerator {

  val algo = BmAlgo(bmSolution)
  val isConstantMult = algo.isConstantMult

  import bmSolution._

  override def implH = ???

  override def implNaiveH =
    Some(new ChainsawOperatorModule(this) {

      val y =
        if (isConstantMult) U(constant.get, widthFull bits)
        else dataIn.last.asUInt()
      val raw = dataIn.head.asUInt() * y

      dataOut.head := {
        multiplierType match {
          case LsbMultiplier => raw.takeLow(widthFull).asUInt
          case MsbMultiplier => raw.takeHigh(widthFull).asUInt
          case FullMultiplier => raw
          case SquareMultiplier => raw
        }
      }.toAFix.d()
    })

  override def latency() = 1

  override def name = s"${if (isConstantMult) "constant" else "variable"}_${className(multiplierType)}_$widthFull"

  override def vivadoUtilEstimation = VivadoUtilEstimation(dsp = bmSolution.dspCost)

  override def fmaxEstimation = 600 MHz

  override def inputTypes =
    if (isConstantMult || multiplierType == SquareMultiplier) Seq(NumericTypeNew.U(widthFull))
    else Seq.fill(2)(NumericTypeNew.U(widthFull))

  override def outputTypes =
    if (multiplierType == MsbMultiplier || multiplierType == LsbMultiplier) Seq(NumericTypeNew.U(widthFull))
    else Seq(NumericTypeNew.U(widthOut))

  override def impl(testCase: TestCase) = {
    val ret = if (isConstantMult) algo.implConstantMult(testCase.data.head.toBigInt())
    else if (multiplierType == SquareMultiplier) algo.impl(testCase.data.head.toBigInt(), testCase.data.head.toBigInt())
    else algo.impl(testCase.data.head.toBigInt(), testCase.data.last.toBigInt())
    Seq(BigDecimal(ret))
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = multiplierType match {
    case MsbMultiplier =>
      val error = golden.head - yours.head
      error.abs < (widthFull / 2)
    case _ => yours.equals(golden)
  }

  override def testCases = Seq.fill(100)(TestCase(randomDataVector))
}
