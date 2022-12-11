package Chainsaw.deprecated

import Chainsaw._
import spinal.core._
import spinal.lib.CounterFreeRun

import scala.language.postfixOps

case class DelayByRam(width: Int, delay: Int) extends ChainsawGenerator {

  require(delay >= 4, s"length too small: $delay")

  val counterWidth = log2Up(delay + 1)
  val paddedLength = Pow2(counterWidth)

  override def name = getAutoName(this)

  override def impl(dataIn: Seq[Any]) = dataIn

  override def inputTypes = Seq(UIntInfo(width))

  override def outputTypes = Seq(UIntInfo(width))

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl

  override def latency = delay

  override def implH = new ChainsawModule(this) {
    val index = CounterFreeRun(paddedLength)
    val ram = Mem(Bits(width bits), paddedLength)
    ram.setAsBlockRam()
    ram.write(index.value, dataIn.head)

    val readAddr = (index.value - U(delay - 3, counterWidth bits)).d()
    dataOut.head := ram.readSync(readAddr).d()
  }
}
