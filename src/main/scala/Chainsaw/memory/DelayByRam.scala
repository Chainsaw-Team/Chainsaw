package Chainsaw.memory

import Chainsaw._
import spinal.core._
import spinal.lib._

case class DelayByRam(width: Int, length: Int) extends ChainsawGenerator {

  require(length >= 4, s"length too small: $length")

  val paddedLength = BigInt(1) << log2Up(length + 1)

  override def name = s"delayByRam_${width}_$length"

  override def impl(dataIn: Seq[Any]) = dataIn

  override var inputTypes = Seq(UIntInfo(width))
  override var outputTypes = Seq(UIntInfo(width))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = length

  override def implH = new ChainsawModule(this) {

    val index = CounterFreeRun(paddedLength)
    val ram = Mem(Bits(width bits), paddedLength)
    ram.setAsBlockRam()
    ram.write(index.value, dataIn.head)

    val readAddr = (index.value - U(length - 3, log2Up(paddedLength) bits)).d()
    dataOut.head := ram.readSync(readAddr).d()
  }
}
