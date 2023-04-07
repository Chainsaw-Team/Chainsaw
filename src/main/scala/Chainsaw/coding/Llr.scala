package Chainsaw.coding

import scala.math._

/** log likelihood ratio
  * @param value
  *   the log likelihood ratio
  */
case class Llr(value: Double) {

  def sign         = if (value > 0) 1 else -1
  def +(that: Llr) = Llr(this.value + that.value)

  /** boxplus operation implemented by log-max approximation
    */
  def ⊞(that: Llr) = {
    // definition
    //    val numerator   = 1 + exp(this.value) * exp(that.value)
    //    val denominator = exp(this.value) + exp(that.value)
    //    LogLikelihood(log(numerator / denominator))
    // approximation
    Llr(this.sign * that.sign * min(this.value.abs, that.value.abs))
  }

  def decide     = value < 0
  def decideBpsk = if (value < 0) -1 else 1

  /** llr of a binary random variable
    *
    * @param p0
    *   probability of symbol 0
    */
  def fromDistribution(p0: Double = 0.5) = Llr(log(p0 / (1 - p0)))

  /** llr of a BPSK+AWGN symbol received
    * @param rx
    *   received symbol
    * @param esn0
    *   Es/N0 of the AWGN channel
    * @param p0
    *   probability of symbol 0
    */
  def fromBpsk(rx: Double, esn0: Double, p0: Double = 0.5) = {
    val fromReceived = Llr(4 * esn0 * rx)
    fromReceived + fromDistribution(p0)
  }
}

object Llr {
  def main(args: Array[String]): Unit = {
    println(Llr(5.6) ⊞ Llr(-7.5))
    println(Llr(0.7) ⊞ Llr(-8.5))
  }
}
