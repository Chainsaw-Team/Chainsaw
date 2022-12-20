package Chainsaw.crypto

import Chainsaw._
import spinal.core._
import arithmetic._
import project.zprize.ZPrizeMSM.baseModulus
import project.zprize.ZPrizeMSM.MPrime

import scala.language.postfixOps

case class Barrett377() extends ModularReduction
  with OverwriteLatency {

  override def widthM = 377

  override def constantModulus = Some(baseModulus)

  // widthIn = 2* widthM
  override def upperBound = if (isConstant) constantModulus.get else (BigInt(1) << widthM) - 1

  val msbSolution = BmSolution(
    baseMultiplier = BaseDspMult(48, 48),
    splits = Seq.fill(3)(2),
    multiplierType = MsbMultiplier, isKaras = Seq.fill(3)(true),
    constant = Some(MPrime << (384 - 378)), threshold = 0)
  val msbGen = Bm(msbSolution)
  val lsbGen = LsbBcm(constant = baseModulus, widthIn = 378, widthOut = 379)
  val cpaGen = Cpa(BinarySubtractor, 379)

  override def implH = new ChainsawOperatorModule(this) {

    val x = dataIn.head.asUInt() // 377 * 2 bits

    val xHighPadded = x.takeHigh(378).asUInt << (384 - 378)
    val xLow = x.takeLow(379).asUInt.d(msbGen.latency() + lsbGen.latency())

    val u = msbGen.process(Seq(xHighPadded.toAFix)).head.asUInt()
    val E = u.takeHigh(378).asUInt
    val r = lsbGen.process(Seq(E.toAFix)).head.asUInt()
    val diff = cpaGen.process(Seq(xLow.toAFix, r.toAFix)).head.asUInt()
    val ret = diff % baseModulus

    dataOut.head := ret.toAFix.truncated
  }

  override def name = "BarrettReduction_" + (if (isConstant) constantModulus.get else s"w$widthM")

  override def fmaxEstimation = 600 MHz

  override def vivadoUtilEstimation = msbGen.vivadoUtilEstimation + lsbGen.vivadoUtilEstimation

  override def latency() = msbGen.latency() + lsbGen.latency()
}
