package Chainsaw.arithmetic

import scala.annotation.tailrec

/** canonical signed digit and corresponding methods
  *
  * @param csd
  *   CSD number represented by a string, 9 stands for -1
  * @see
  *   ''Digital Signal Processing with Field Programmable Gate Arrays'' Chapter 2.2.2
  */
case class Csd(csd: String) {
  def evaluate: BigInt = csd.reverse.zipWithIndex.map { case (digit, i) =>
    digit match {
      case '0' => BigInt(0)
      case '1' => BigInt(1) << i
      case '9' => -(BigInt(1) << i)
    }
  }.sum

  def weight: Int = csd.count(_ != '0') // number of nonzero digits

  def takeLow(value: Int) = Csd(csd.takeRight(value))

  def takeHigh(value: Int) = Csd(csd.take(value))

  override def toString = csd
}

object Csd {
  def getWeight(bigInt: BigInt) = bigInt.toString(2).count(_ != '0')

  // TODO: this is the simple csd coding, use optimal csd coding
  def apply(bigInt: BigInt): Csd = {
    val raw     = bigInt.toString(2).reverse + "0" // LSB -> MSB with 0 padded
    val pattern = "11+0".r

    @tailrec
    def process(raw: String): String = {
      pattern.findFirstIn(raw) match {
        case None    => raw.reverse
        case Some(x) => process(raw.replaceFirst(x, "9" + "0" * (x.length - 2) + "1"))
      }
    }

    val str    = process(raw)
    val rawStr = if (str.startsWith("0")) str.tail else str // remove the leading 0
    val ret    = Csd(process(rawStr.reverse))               // MSB -> LSB
    assert(ret.evaluate == bigInt, s"ret = ${ret.evaluate}, bigInt = $bigInt")
    ret
  }
}
