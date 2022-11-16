package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._
import Chainsaw.dag._

/** barrett modular multiplier
 *
 * @param widthIn         width of multiplicands
 * @param constantModulus Some when modulus is fixed, None when it is not
 * @see [[BarrettFineAlgo]]
 */
case class Barrett(widthIn: Int, constantModulus: Option[BigInt] = None) extends Dag {

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

  implicit class karatsubaUtil(port: DagPort) {
    def splitAt(width: Int) = {
      val s = Split(port.width, width).asVertex
      s := port
      (s.out(0), s.out(1))
    }

    def takeLow(width: Int) = port.splitAt(width)._2

    def takeHigh(width: Int) = port.splitAt(port.width - width)._1
  }

  /** --------
   * dag implementation
   * -------- */

  constantModulus match {
    case Some(modulus) => // constant modulus version
      val Seq(a, b) = Seq.fill(2)(InputVertex(UIntInfo(k)))
      val o = OutputVertex(UIntInfo(k))

      val mPrime = (BigInt(1) << (2 * modulus.bitLength)) / modulus

      val algo = BarrettFineAlgo(modulus)
      val msbWidthInvolved = algo.multMsb.widthInvolved
      val lsbWidthInvolved = algo.multLsb.widthInvolved

      // declaration
      val multFull = Karatsuba(k, constantModulus).asVertex
      //      val multFull = KaratsubaFake(k, constantModulus).asVertex
      val multMsb = MsbBcm(mPrime, widthIn = k + 1, widthInvolved = msbWidthInvolved, widthOut = k + 1, useCsd = true).asVertex
      val add0 = CpaS2S(BinaryAdder, k + 1, withCarry = false).asVertex // cpa after multMsb

      val multLsb = LsbBcm(mPrime, widthIn = k + 1, widthOut = lsbWidthInvolved, useCsd = true).asVertex
      val add1 = CpaS2S(BinaryAdder, lsbWidthInvolved, withCarry = false).asVertex // cpa after multLsb
      val sub = CpaS2S(BinarySubtractor, lsbWidthInvolved, withCarry = false).asVertex // cpa before reduction

      val reduction = FineReduction(modulus, 10).asVertex // fine reduction

      // connection
      multFull := (a, b)
      multMsb := multFull.out(0).takeHigh(k + 1)

      val F = multMsb >> add0 >> multLsb >> add1
      val NLow = multFull.out(0).takeLow(lsbWidthInvolved)

      sub := (NLow, F.out(0))

      sub >> reduction

      o := reduction.out(0)
    case None => ???
  }

  this.exportPng("barrett")
  logger.info(s"graph structure: ${this.toString}")

  graphDone()
}

object Barrett extends App {
  val barrettGen = Barrett(377, Some(project.zprize.ZPrizeMSM.baseModulus))
  barrettGen.setVerticesAsNaive()
  ChainsawSynth(barrettGen, "synthBarrett")
}
