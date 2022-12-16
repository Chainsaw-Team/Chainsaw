package Chainsaw.deprecated

import Chainsaw.arithmetic.{BmSolution, MultSearch}
import Chainsaw.crypto.BarrettFineAlgo
import Chainsaw._

import scala.util.Random

/** barrett modular multiplier
 *
 * @param widthIn         width of multiplicands
 * @param constantModulus Some when modulus is fixed, None when it is not
 * @see [[BarrettFineAlgo]]
 */
case class Barrett(widthIn: Int, constantModulus: Option[BigInt] = None, multiplierType: MultiplierType)
  extends Dag {

  val k = widthIn

  require(constantModulus.forall(_.bitLength == k))

  override def name = getAutoName(this)

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = constantModulus match {
      case Some(modulus) => data.product % modulus
      case None =>
        val Seq(a, b, modulus, _) = data
        a * b % modulus
    }
    Seq(ret)
  }

  override def generateTestCases = {
    constantModulus match {
      case Some(modulus) =>
        val candidates = Seq.fill(1000)(BigInt(widthIn, Random)).filter(_ < modulus)
        val data = candidates.take((candidates.length.divideAndCeil(2) - 1) * 2)
        if (multiplierType != SquareMultiplier) data
        else data.flatMap(x => Seq(x, x))
      case None => ???
    }
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
      val solution = MultSearch.getBmParetos(k, multiplierType).minBy(_.dspCost)
      val multFull = deprecated.Bm(k, None, solution.asInstanceOf[BmSolution]).asVertex
      val multMsb = MsbBcm(algo.MPrime, widthIn = k + 1, widthInvolved = msbWidthInvolved, widthOut = k + 1, useCsd = true).asVertex
      val multLsb = LsbBcm(modulus, widthIn = k + 1, widthOut = lsbWidthInvolved, useCsd = true).asVertex
      val sub = CpaS2S(TernarySubtractor1, lsbWidthInvolved, withCarry = false).asVertex // cpa before reduction
      val reduction = FineReduction(modulus, 10).asVertex // fine reduction

      multFull := (a, b)
      multMsb := multFull.out(0).takeHigh(k + 1)
      multLsb <-< multMsb

      val F = multLsb.out(0)
      val NLow = multFull.out(0).takeLow(lsbWidthInvolved)
      sub := (NLow, constantC, F) // NLow + C - F
      reduction := sub.out(0).resize(k + 4)
      o := reduction.out(0)

    case None =>
      val Seq(a, b, m) = Seq.fill(3)(InputVertex(UIntInfo(k)))
      val mPrime = InputVertex(UIntInfo(k + 1))
      val o = OutputVertex(UIntInfo(k))

      val solution0 = MultSearch.getBmParetos(k, multiplierType).minBy(_.dspCost)
      val multFull = deprecated.Bm(k, None, solution0.asInstanceOf[BmSolution]).asVertex
      val solution1 = MultSearch.getBmParetos(k + 1, FullMultiplier).minBy(_.dspCost)
      val multMsb = deprecated.Bm(k + 1, None, solution1.asInstanceOf[BmSolution]).asVertex
      val solution2 = MultSearch.getBmParetos(k + 1, FullMultiplier).minBy(_.dspCost)
      val multLsb = deprecated.Bm(k + 1, None, solution2.asInstanceOf[BmSolution]).asVertex
      val sub = CpaS2S(BinarySubtractor, k + 4, withCarry = false).asVertex // cpa before reduction
      // FIXME: fineReduction for variable modulus
      val reduction = FineReduction((BigInt(1) << 376) + 1, 10).asVertex // fine reduction

      multFull := (a, b)
      multMsb := (multFull.out(0).takeHigh(k + 1), mPrime)
      multLsb := (multMsb.out(0).takeHigh(k + 1), m)
      val F = multLsb.out(0).takeLow(k + 2)
      val NLow = multFull.out(0).takeLow(k + 2)
      sub := (NLow, F) // NLow - F
      reduction := sub.out(0).resize(k + 4)
      o := reduction.out(0)
  }

  this.exportPng("barrett")
  logger.info(s"graph structure: ${this.toString}")

  graphDone()
}
