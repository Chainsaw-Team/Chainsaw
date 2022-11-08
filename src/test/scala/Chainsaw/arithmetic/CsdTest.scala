package Chainsaw.arithmetic

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import Chainsaw._

class CsdTest extends AnyFlatSpec {

  val testCount = 10000
  val testWidth = 256
  val testData = Seq.fill(testCount)(BigInt(testWidth, Random))

  "Csd" should "work correctly" in testData.foreach(data => assert(Csd(data).evaluate == data))

  it should "work efficiently" in {
    val weightBefore = testData.map(_.toString(2).count(_ != '0')).sum
    val weightAfter = testData.map(data => Csd(data).weight).sum
    logger.info(s"average compression rate = ${weightAfter.toDouble / weightBefore}")
  }
}
