package Chainsaw.edaFlow.boards.alinx

import Chainsaw.edaFlow.boards.FmcLpc
import spinal.core._

import scala.language.postfixOps

/** */
class Alinx40Pin() extends Bundle { // TODO: pull request to spinal.lib.bus.alinx
  // FIXME: "bools" in same bits must have same direction
  val P, N                 = Bits(17 bits) // P = 3,5,7...35, N = 4,6,8...36
  val validRange: Seq[Int] = 3 to 36

  def apply(idx: Int): Bool = {
    assert(validRange.contains(idx), "pin idx out of range")
    if (idx % 2 == 1) P(idx / 2 - 1) else N(idx / 2 - 2)
  }

  def foreach(f: (Int, Bool) => Unit): Unit = validRange.foreach(i => f(i, apply(i)))
}

object Alinx40Pin {
  def apply(): Alinx40Pin = new Alinx40Pin()
}

case class AN9767() extends Component {
  val channel1, channel2                                 = in UInt (14 bits)
  val channel1Wrt, channel1Clk, channel2Wrt, channel2Clk = in Bool ()
  val alinx40PinOut                                      = out(Alinx40Pin())
  alinx40PinOut.assignDontCare()

  val channel1Seq: Seq[Bool] = (0 until 7).map(i => Seq(alinx40PinOut.N(i), alinx40PinOut.P(i))).flatten.reverse
  val channel2Seq: Seq[Bool] = (9 until 16).map(i => Seq(alinx40PinOut.N(i), alinx40PinOut.P(i))).flatten.reverse
  // connection
  channel1Seq.zipWithIndex.foreach { case (pin, index) => pin := channel1(index) }
  channel2Seq.zipWithIndex.foreach { case (pin, index) => pin := channel2(index) }
  alinx40PinOut.N(7) := channel1Wrt
  alinx40PinOut.P(7) := channel1Clk
  alinx40PinOut.N(8) := channel2Clk
  alinx40PinOut.P(8) := channel2Wrt
}

case class AN9238() extends Component {
  val alinx40Pin               = Alinx40Pin()
  val channel1, channel2       = out UInt (12 bits)
  val channel1Clk, channel2Clk = in Bool ()
  val channel1Otr, channel2Otr = out Bool ()

  alinx40Pin.foreach { case (i, pin) =>
    i match {
      case 5  => out(pin)    := channel2Clk
      case 31 => out(pin)    := channel1Clk
      case 16 => channel2Otr := in(pin)
      case 32 => channel1Otr := in(pin)
      case _ =>
        if (i < 16) channel2(i - 4) := in(pin)
        else {
          val idx = if (i % 2 == 0) i - 20 else i - 18
          channel1(idx) := in(pin)
        }
    }
  }
}

object AN9238 extends App {
  SpinalVerilog(AN9238())
}

object AN9767 {
  def getVoltageValue(voltage: Double): UInt = {
    // -4V -> 0x0000, 4V -> 0x3FFF
    val step  = (0x3fff - 0x0000) / (4.0 - (-4.0))
    val value = ((voltage - (-4.0)) * step + 0x0000).toInt
    println(s"voltage: $voltage, value: $value")
    U(value, 14 bits)
  }
}

object FL1010 {

  val order0 = Seq(15, 16, 11, 0, 2, 3, 12, 7, 8, 4, 14, 13, 9, 10, 5, 6, 1)
  val order1 = Seq(17, 18, 23, 26, 27, 28, 29, 24, 25, 21, 22, 31, 30, 33, 32, 19, 20)
  def apply(j1FmcLpc: FmcLpc): (Alinx40Pin, Alinx40Pin) = {

    val j240Pin, j340Pin = Alinx40Pin()

    // connections
    for (i <- 0 until 17) {
      j340Pin.P(i) := j1FmcLpc.LA_P(order0(i))
      j340Pin.N(i) := j1FmcLpc.LA_N(order0(i))
      j240Pin.P(i) := j1FmcLpc.LA_P(order1(i))
      j240Pin.N(i) := j1FmcLpc.LA_N(order1(i))
    }

    (j240Pin, j340Pin)
  }

  def apply(j240Pin: Alinx40Pin, j340Pin: Alinx40Pin): FmcLpc = {

    val j1FmcLpc = FmcLpc()

    // connections
    for (i <- 0 until 17) {
      j1FmcLpc.LA_P(order0(i)) := j340Pin.P(i)
      j1FmcLpc.LA_N(order0(i)) := j340Pin.N(i)
      j1FmcLpc.LA_P(order1(i)) := j240Pin.P(i)
      j1FmcLpc.LA_N(order1(i)) := j240Pin.N(i)
    }

    j1FmcLpc
  }

}
