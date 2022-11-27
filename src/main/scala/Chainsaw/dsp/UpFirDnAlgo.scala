package Chainsaw.dsp

import Chainsaw._

import scala.util.Random

case class UpFirDnAlgo(coeffs: Array[Double], up: Int, down: Int) {

  def golden(dataIn: Signal): Signal = matlabEngine
    .feval("upfirdn", dataIn, coeffs, Array(up.toDouble), Array(down.toDouble)).asInstanceOf[Signal]

  def naiveImpl(dataIn: Signal): Signal = {
    val upSampled = upSample(dataIn, up)
    val convolved = conv(upSampled, coeffs)
    downSample(convolved, down)
  }

  // upfirdn implemented by polyphase decomposition
  // TODO: for any integer up, down and parallel, the polyphase decomposition can be implemented
  def polyPhaseImpl(dataIn: Signal) = ???

  def selfTest(): Unit = {
    val dataIn = Array.fill(1000)(Random.nextDouble())
    assert(getCorr(naiveImpl(dataIn), golden(dataIn)) > 0.9) // naiveImpl is consistent with golden(Matlab)
  }
}

object UpFirDnAlgo {
  def main(args: Array[String]): Unit = {
    val coeffs = Array.fill(100)(Random.nextDouble())
    val up = 2
    val dn = 3
    val algo = UpFirDnAlgo(coeffs, up, dn)
    algo.selfTest()
  }
}
