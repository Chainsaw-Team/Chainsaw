package Chainsaw

import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt

/** this class providing methods for you to manipulate BigInt just like Bits
  * @param value
  *   the BigInt
  * @param width
  *   upper width of the BigInt
  */
case class BitValue(value: BigInt, width: Int) {

  /** works the same as SpinalHDL splitAt on Bits
    *
    * @example
    *   10100.split(3) = (10,100)
    */
  def splitAt(lowWidth: Int): (BigInt, BigInt) = {
    require(value >= 0, s"$value")
    val base = BigInt(1) << lowWidth
    (value >> lowWidth, value % base)
  }

  // represent a negative BigInt by 2's complement
  def to2sComplement =
    if (value >= 0) value
    else (BigInt(1) << width) + value

  def takeLow(n: Int) = splitAt(n)._2

  def takeHigh(n: Int) = splitAt(width - n)._1

  def apply(range: Range) = {
    (value / pow2(range.low)) % pow2(range.length)
  }

  // TODO: by bitCount and Slice
  /** works the same as SpinalHDL subdivideIn on Bits
    */
  def subdivideIn(n: Int): Seq[BigInt] = {
    val padded       = BitValue(value, width.nextMultipleOf(n))
    val segmentWidth = width.divideAndCeil(n)
    val segments     = ArrayBuffer[BigInt]()
    var current      = padded
    (0 until n - 1).foreach { i =>
      val (high, low) = current.splitAt(segmentWidth)
      segments += low
      current = high.toBitValue(segmentWidth * (n - i - 1))
    }
    segments += current.value
    segments
  }

  def ##(that: BitValue): BigInt = (this.value << that.width) + that.value
  def @@(that: BitValue): BitValue =
    BitValue((this.value << that.width) + that.value, this.width + that.width)

  def asBools: Seq[BigInt] = value
    .toString(2)
    .reverse
    .padTo(width, '0')
    .map(char => BigInt(char.asDigit))

  def unary_~ : BigInt = (BigInt(1) << width) - 1 - value
}
