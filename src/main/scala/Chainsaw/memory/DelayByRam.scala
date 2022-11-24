package Chainsaw.memory

import Chainsaw._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DelayByRam(width: Int, length: Int) extends ChainsawGenerator {

  require(length >= 4, s"length too small: $length")

  val paddedLength = BigInt(1) << log2Up(length + 1)

  override def name = s"delayByRam_${width}_$length"

  override def impl(dataIn: Seq[Any]) = dataIn

  override def inputTypes = Seq(UIntInfo(width))

  override def outputTypes = Seq(UIntInfo(width))

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl

  override def latency = length

  override def implH = new ChainsawModule(this) {

    val index = CounterFreeRun(paddedLength)
    val ram = Mem(Bits(width bits), paddedLength)
    ram.setAsBlockRam()
    ram.write(index.value, dataIn.head)

    val readAddr = (index.value - U(length - 3, log2Up(paddedLength) bits)).d()
    dataOut.head := ram.readSync(readAddr).d()
  }
}
