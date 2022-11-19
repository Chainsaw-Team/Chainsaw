package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._
import Chainsaw.dag._
import Chainsaw.xilinx._
import scala.util.Random

/** barrett modular multiplier
 *
 * @param widthIn         width of multiplicands
 * @param constantModulus Some when modulus is fixed, None when it is not
 * @see [[BarrettFineAlgo]]
 */
case class Barrett(widthIn: Int, constantModulus: Option[BigInt] = None)
  extends Dag {

  val k = widthIn

  require(constantModulus.forall(_.bitLength == k))

  override def name = s"Barrett_w_$k"

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = constantModulus match {
      case Some(modulus) => data.product % modulus
      case None =>
        val Seq(a, b, modulus) = data
        a * b % modulus
    }
    Seq(ret)
  }

  override def generateTestCases = {
    val candidates = Seq.fill(1000)(BigInt(widthIn, Random)).filter(_ < constantModulus.get)
    candidates.take((candidates.length.divideAndCeil(2) - 1) * 2)
  }

  /** --------
   * dag implementation
   * -------- */
  constantModulus match {
    case Some(modulus) => // constant modulus version

      val algo = BarrettFineAlgo(modulus)
      val msbWidthInvolved = algo.multMsb.widthInvolved
      val lsbWidthInvolved = algo.multLsb.widthInvolved

      // declaration of I/Os
      val Seq(a, b) = Seq.fill(2)(InputVertex(UIntInfo(k)))
      val constantC = ConstantVertex(UIntInfo(lsbWidthInvolved), algo.C)
      val o = OutputVertex(UIntInfo(k))

      // declaration of operators
      val multFull = Karatsuba(k, strategy = DspFirst).asVertex
      val multMsb = MsbBcm(algo.MPrime, widthIn = k + 1, widthInvolved = msbWidthInvolved, widthOut = k + 1, useCsd = true).asVertex
      val multLsb = LsbBcm(modulus, widthIn = k + 1, widthOut = lsbWidthInvolved, useCsd = true).asVertex
      val sub = CpaS2S(TernarySubtractor1, lsbWidthInvolved, withCarry = false).asVertex // cpa before reduction
      val reduction = FineReduction(modulus, 10).asVertex // fine reduction

      // connection
      multFull := (a, b)
      multMsb := multFull.out(0).takeHigh(k + 1)
      multLsb <-< multMsb
      val F = multLsb.out(0)
      val NLow = multFull.out(0).takeLow(lsbWidthInvolved)
      sub := (NLow, constantC, F) // NLow + C - F
      reduction := sub.out(0).resize(k + 4)

      o := reduction.out(0)
    // TODO: when modulus is not a constant
  }

  this.exportPng("barrett")
  logger.info(s"graph structure: ${this.toString}")

  graphDone()
}

object Barrett extends App {
  val barrettGen = Barrett(377, Some(project.zprize.ZPrizeMSM.baseModulus))
  ChainsawSynth(barrettGen, "synthBarrett")
}
