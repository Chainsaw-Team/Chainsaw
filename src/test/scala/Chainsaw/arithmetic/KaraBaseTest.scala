package Chainsaw.arithmetic

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class KaraBaseTest extends AnyFlatSpec {

  def testKaraBase(mode: MultiplierType) = {
    val widthA = 8
    val widthB = if (mode == Kara) 10 else 8
    val data = Seq.fill(1000)(Seq(BigInt(widthA, Random), BigInt(widthA, Random), BigInt(widthB, Random), BigInt(widthB, Random))).flatten
    val gen = KaraBase(widthA, widthB, mode)
    ChainsawTest("testKaraBase", gen, data).doTest()
  }

  val modes = Seq(FullMultiplier, SquareMultiplier, MsbMultiplier, LsbMultiplier, Kara)
//  val modes = Seq(SquareMultiplier)

  "kara base" should "work" in modes.foreach(testKaraBase)

}