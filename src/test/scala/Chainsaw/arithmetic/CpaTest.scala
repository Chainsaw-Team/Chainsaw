package Chainsaw.arithmetic

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CpaTest extends AnyFlatSpec {

  behavior of "Cpa Mode Test"

  def testCpaMode(mode: CpaMode): Unit = {
    it should s"work correctly on ${mode.getClass.getSimpleName}" in {
      val data = Seq.fill(1000)(BigInt(10, Random))
      val gen = Cpa(BinaryAdder, Seq.fill(4)(10), mode, withCarry = true)
      ChainsawTest("testAdd", gen, data).doTest()
    }
  }

  Seq(M2S, S2M, S2S, M2M).foreach(testCpaMode)

  behavior of "adder Mode Test"

  def testAdderMode(mode: AdderType): Unit = {
    it should s"work correctly on ${mode.getClass.getSimpleName}" in {
      val data = Seq.fill(1200)(BigInt(10, Random))
      val gen0 = Cpa(mode, Seq.fill(4)(10), S2S, withCarry = true)
      val gen1 = Cpa(mode, Seq.fill(4)(10), S2S, withCarry = false)
      ChainsawTest("testCpa", gen0, data).doTest()
      ChainsawTest("testCpa", gen1, data).doTest()
    }
  }

  Seq(BinaryAdder, BinarySubtractor,
    //    TernaryAdder, TernarySubtractor1, TernarySubtractor2 // TODO: implementation of ternary adder
  ).foreach(testAdderMode)

  // TODO: a high dimensional matrix test for all configurations
}