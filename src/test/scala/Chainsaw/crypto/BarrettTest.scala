package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BarrettTest extends AnyFlatSpec {

  it should "work" in { // verilator compilation takes 18 minutes
    val data = Seq.fill(1000)(BigInt(377, Random))
    val modulus = Chainsaw.project.zprize.ZPrizeMSM.baseModulus
    val gen = Barrett(377, Some(modulus))
    setAsNaive(Bcm) // when bcm is set as naive, this takes less than 2 minutes
    ChainsawTest("testBarrett377", gen, data).doTest()
  }

  //  it should "work" in {
  //    val data = Seq.fill(1000)(BigInt(64, Random))
  //    val modulus = (BigInt(1) << 63) + 1
  //    ChainsawTest("testBarrett64", Barrett(64, Some(modulus)), data).doTest()
  //  }
}
