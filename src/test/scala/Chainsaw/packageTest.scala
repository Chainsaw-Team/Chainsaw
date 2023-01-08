package Chainsaw

import org.scalatest.flatspec.AnyFlatSpec

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class packageTest extends AnyFlatSpec {

  behavior of "BigIntUtil"

  it should "do slicing correctly" in {
    assert(BigInt("11011", 2).toBitValue(5).takeHigh(3) == BigInt("110", 2))
    assert(BigInt("11011", 2).toBitValue(5).takeLow(3) == BigInt("011", 2))
    assert(BigInt("11011", 2).toBitValue(5)(3 downto 1) == BigInt("101", 2))

    assert(
      BigInt("11010011", 2).toBitValue().subdivideIn(2) ==
        Seq(BigInt("1101", 2), BigInt("0011", 2)).reverse
    ) // without padding

    assert(
      BigInt("11011", 2).toBitValue().subdivideIn(3) ==
        Seq(BigInt("01", 2), BigInt("10", 2), BigInt("11", 2)).reverse
    ) // with padding
  }

  it should "generate 2's complement correctly" in {
    assert(BigInt(-3).toBitValue(3).to2sComplement == BigInt("101", 2))
    assert(BigInt(-7).toBitValue(4).to2sComplement == BigInt("1001", 2))
  }

}
