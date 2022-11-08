package Chainsaw.arithmetic

import spinal.core._

/** describe the attributes of an UInt Operand needed for bit heap compression
 *
 * @param width
 * width of the operand
 * @param weight
 * weight of the operand
 * @param isPositive
 * signedness of the operand
 * @param time
 * delay of the operand
 */
case class ArithInfo(width: Int, weight: Int, isPositive: Boolean = true, time: Int = 0) {
  val low = weight
  val high = low + width - 1
  val range = high downto low

  def <<(shiftLeft: Int) = ArithInfo(width, weight + shiftLeft, isPositive)

  def >>(shiftRight: Int): ArithInfo = <<(-shiftRight)

  def unary_- = ArithInfo(width, weight, !isPositive)
}
