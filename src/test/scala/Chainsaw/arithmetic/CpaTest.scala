package Chainsaw.arithmetic

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CpaTest extends AnyFlatSpec {
  behavior of "Cpa Mode Test"

  def testCpaFuncInMode(adder: AdderType, mode: CpaMode, useNaive: Boolean = false): Unit = {
    it should s"work correctly on ${adder.getClass.getSimpleName.init} by ${mode.getClass.getSimpleName.init}" in {
      val data = Seq.fill(1200)(BigInt(10, Random))
      val gen0 = Cpa(adder, Seq.fill(4)(10), mode, withCarry = true)
      val gen1 = Cpa(adder, Seq.fill(4)(10), mode, withCarry = false)
      // use Naive impl
      if (useNaive) Seq(gen0, gen1).foreach(_.setAsNaive())
      ChainsawTest(s"testCpa_${if (useNaive) "useNaive" else "useImplH"}_withCarry", gen0, data).doTest()
      ChainsawTest(s"testCpa_${if (useNaive) "useNaive" else "useImplH"}_withoutCarry", gen1, data).doTest()
    }
  }

  val adderTypes = Seq(BinaryAdder, BinarySubtractor, TernaryAdder, TernarySubtractor1, TernarySubtractor2)
  val cpaModes   = Seq(M2M, M2S, S2M, S2S)
  cpaModes.foreach { mode => adderTypes.foreach(adder => testCpaFuncInMode(adder, mode, useNaive = false)) }

}
