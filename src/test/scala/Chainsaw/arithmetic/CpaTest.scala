package Chainsaw.arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw._

import scala.util.Random

class CpaTest extends AnyFlatSpec {

  "cpa" should "work under 4 different pipeline modes" in {
    val m2s = Cpa(BinaryAdder, Seq.fill(4)(10), M2S)
    val s2m = Cpa(BinaryAdder, Seq.fill(4)(10), S2M)
    val s2s = Cpa(BinaryAdder, Seq.fill(4)(10), S2S)
    val m2m = Cpa(BinaryAdder, Seq.fill(4)(10), M2M)

    val data = Seq.fill(1000)(BigInt(10, Random))
    ChainsawTest("testS2S", s2s, data).doTest()
    ChainsawTest("testS2M", s2m, data).doTest()
    ChainsawTest("testM2S", m2s, data).doTest()
    ChainsawTest("testM2M", m2m, data).doTest()
  }


  it should "work under 5 different adder modes" in {

    val widths = Seq.fill(4)(10)

    val add = Cpa(BinaryAdder, widths, M2M)
    val sub = Cpa(BinarySubtractor, widths, M2M)
    val ter0 = Cpa(TernaryAdder, widths, M2M)
    val ter1 = Cpa(TernarySubtractor1, widths, M2M)
    val ter2 = Cpa(TernarySubtractor2, widths, M2M)

    val data = Seq.fill(1200)(BigInt(10, Random))
    Seq(
      add,
      sub,

      // TODO: implementation of ternary adder
      //      ter0,
      //      ter1,
      //      ter2

    ).foreach(gen => ChainsawTest("testCpa", gen, data).doTest())
  }

}
