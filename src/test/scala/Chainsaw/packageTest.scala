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
  }

}
