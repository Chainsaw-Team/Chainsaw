package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._
import cc.redberry.rings.scaladsl._
import spinal.core._

import scala.util.Random

abstract class ModularMult {

  val M: BigInt

  def impl(x: BigInt, y: BigInt): BigInt

  def selfTest(): Unit = {
    val k = M.bitLength
    //    val pairCount = 1000000
    val pairCount = 1000
    logger.info(s"self testing with $pairCount pairs of random input")
    val data = Seq.fill(pairCount)(BigInt(k, Random), BigInt(k, Random))
    data.foreach { case (x, y) => assert(impl(x, y) == (x * y) % M) }
    logger.info(s"self testing done!")
  }
}

case class BarrettAlgo(override val M: BigInt) extends ModularMult {

  val k      = M.bitLength
  val MPrime = (BigInt(1) << (2 * k)) / M

  override def impl(x: BigInt, y: BigInt): BigInt = {
    require(x.bitLength <= k, y.bitLength <= k)
    // pre-computation
    val N = (x * y).toBitValue(2 * k)                          // mult0
    val u = (MPrime * N.takeHigh(k + 1)).toBitValue(2 * k + 2) // mult1
    val E = u.takeHigh(k + 1)

    val T = N.value - M * E // mult2
    // bound verification
    val reductionMax = 3
    assert(T < reductionMax * M && T >= 0, s"T = $T")

    Zp(M)(T).toBigInt
  }
}

/** fine-tuned Barrett algorithm taking advantage of MSB/LSB truncated
  * multiplication
  *
  * @param M
  *   modulus
  * @see
  *   ''Langhammer, Martin and Bogdan Mihai Pasca. “Efficient FPGA Modular
  *   Multiplication Implementation.” The 2021 ACM/SIGDA International Symposium
  *   on Field-Programmable Gate Arrays (2021): n. pag.''
  */
case class BarrettFineAlgo(override val M: BigInt) extends ModularMult {

  /** -------- preparation
    * --------
    */
  val k      = M.bitLength
  val MPrime = (BigInt(1) << (2 * k)) / M

  val accuracy = 10
  val multMsb = (k + 1 until k + 10)
    .map(width =>
      BcmAlgo(MPrime, MsbMultiplier, k + 1, width, k + 1, useCsd = true)
    )
    .dropWhile(mult => (mult.upperBound - mult.lowerBound) > accuracy)
    .head
  logger.info(s"width involved in msbMult: ${multMsb.widthInvolved}")
  val errorMax = multMsb.upperBound
  val errorMin = multMsb.lowerBound
  val reductionMax =
    3 + errorMax - errorMin // 3 from the original implementation
  val widthComp = k + log2Up(reductionMax)
  val C         = Zp(pow2(widthComp))((-errorMin) * M).toBigInt
  val multLsb =
    BcmAlgo(M, LsbMultiplier, k + 1, widthComp, widthComp, useCsd = true)
  logger.info(s"width involved in lsbMult: ${multLsb.widthInvolved}")

  override def impl(x: BigInt, y: BigInt) = {

    require(x.bitLength <= k, y.bitLength <= k)
    val N    = (x * y).toBitValue(2 * k) // mult0
    val NLow = N.takeLow(widthComp)

    // truncated msb mult, which introduce extra error on E
    val E = multMsb.impl(N.takeHigh(k + 1)) // mult1

    // truncated lsb mult
    val F = multLsb.impl(E) // mult2

    // TODO: analysis
    // fine reduction, inputs of fine reduction are NLow, F and C
    val T = Zp(pow2(widthComp))(NLow - F + C).toBigInt // no cost on hardware

    // bound verification
    assert(T < reductionMax * M && T >= 0, s"\nT = $T \nT/M = ${T / M}")

    Zp(M)(T).toBigInt
  }
}
