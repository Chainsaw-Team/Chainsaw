package Chainsaw.device

import Chainsaw.{ChainsawFlatSpec, TestConfig}
import spinal.core._

import scala.util.Random

class DeviceIpTests extends ChainsawFlatSpec {

  def testLUT6_2(): Unit = {

    // test the factory method from logic expression to LUT6_2
    val exp0 =
      (i0: Boolean, i1: Boolean, i2: Boolean, i3: Boolean, i4: Boolean) =>
        (i0.toInt + i1.toInt + i2.toInt) >= 2

    val exp1 =
      (i0: Boolean, i1: Boolean, i2: Boolean, i3: Boolean, i4: Boolean) =>
        i0 ^ i1 ^ i2 ^ i3

    val gen = LUT5to2(exp0, exp1)
    testOperator(gen, generatorConfigTable("LUT6_2"))

  }

  behavior of "LUT6_2 utils"

  it should "get inversed" in {
    val valueBefore = BigInt(64, Random)
    val inverseMask = Seq(false, true, false, false, false, false)
    val valueAfter  = LUT6_2.getValueWithInverse(valueBefore, inverseMask)

    (0 until 100).foreach { _ =>
      val bools = Seq.fill(6)(Random.nextInt(2)) // low to high
      val addr  = BigInt(bools.reverse.mkString(""), 2)
      val boolsMasked =
        bools.zip(inverseMask).map { case (i, bool) => if (bool) 1 - i else i }
      val addrMasked = BigInt(boolsMasked.reverse.mkString(""), 2)
      assert(
        valueAfter
          .toString(2)
          .reverse
          .padTo(64, '0')
          .apply(addr.toInt) == valueBefore
          .toString(2)
          .reverse
          .padTo(64, '0')
          .apply(addrMasked.toInt),
        s"addr = $addr = ${addr.toString(2).reverse.padTo(6, '0')}, addrMasked = $addrMasked = ${addrMasked.toString(2).reverse.padTo(6, '0')}"
      )
    }
  }

  override def generatorConfigTable = Map(
    "LUT6_2" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = true
    )
  )

  testLUT6_2()
}
