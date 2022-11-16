package Chainsaw.arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw._
import Chainsaw.xilinx._

import scala.util.Random

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class KaratsubaTest extends AnyFlatSpec {

  val karaData = Seq.fill(1000)(BigInt(377, Random))

  val karaGen = Karatsuba(377)

  "Karatsuba" should "gen" in SpinalConfig().generateVerilog(karaGen.implH)

  "Karatsuba" should "work" in {
    karaGen.setAsNaive()
    ChainsawTest("testKara377", Karatsuba(377), karaData).doTest()
  }

}