package Chainsaw

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class NumericTypeTest extends AnyFlatSpec {

  behavior of "from BigInt and to BigInt"

  def conversionTest(numericType: NumericType): Unit = {
    val width = numericType.bitWidth
    val data = Seq.fill(10000)(BigInt(width, Random))
    data.foreach(ele => assert(numericType.toBigInt(numericType.fromBigInt(ele)) == ele))
  }

  it should "work correctly for signed int" in conversionTest(SIntInfo(16))
  it should "work correctly for signed fix" in conversionTest(SFixInfo(8, 8))
  it should "work correctly for complex fix" in conversionTest(ComplexFixInfo(8, 8))

}
