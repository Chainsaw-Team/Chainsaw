package Chainsaw.device

import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.math._
import org.slf4j._

object LUTUtils {
  def expression2value(expression: Seq[Boolean] => Boolean, bitCount: Int) = {
    val truthTable = ArrayBuffer[Int]()
    (0 until (1 << bitCount)) // build truth table
      .map { id =>
        val bools = id.toBinaryString.reverse.padTo(bitCount, '0').map(_ == '1')
        bools
      } // low to high
      .map(expression)
      .foreach(bool => truthTable += bool.toInt)

    BigInt(truthTable.reverse.mkString, 2)
  }

  def getExpressionWithInverse(
      expression: Seq[Boolean] => Boolean,
      inverseList: Seq[Boolean]
  ): Seq[Boolean] => Boolean = { data =>
    {
      val inversed = data.zip(inverseList).map { case (bool, inverse) =>
        if (inverse) !bool else bool
      }
      expression(inversed)
    }
  }

  def getValueWithInverse(value: BigInt, inverseList: Seq[Boolean]): BigInt = {
    val logger = LoggerFactory.getLogger("ValueWithInverse")
    if (value.bitLength > pow(2, inverseList.length).toInt)
      logger.warn(
        s"the inverseList is incomplete, the value will be cast"
      )
    val truthTable = value.toString(2).reverse.padTo(pow(2, inverseList.length).toInt, '0') // low to high
    val inversedTruthTable = (0 until pow(2, inverseList.length).toInt).map { addr =>
      val addrStr = addr.toBinaryString.reverse
        .padTo(inverseList.length, '0')
        .zip(inverseList)
        .map { case (bit, inverse) =>
          if (inverse) 1 - bit.asDigit else bit.asDigit
        }
        .reverse
        .mkString
      val inversedAddr = BigInt(addrStr, 2)
      truthTable(inversedAddr.toInt)
    } // low to high
    val ret = BigInt(inversedTruthTable.reverse.mkString, 2)
    ret
  }

}
