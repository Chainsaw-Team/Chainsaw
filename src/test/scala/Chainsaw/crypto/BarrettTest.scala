package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BarrettTest extends AnyFlatSpec {


    it should "work" in {
      val data = Seq.fill(1000)(BigInt(64, Random))
      val modulus = (BigInt(1) << 63) + 1
      ChainsawTest("testBarrett64", Barrett(64, Some(modulus)), data).doTest()
    }
}
