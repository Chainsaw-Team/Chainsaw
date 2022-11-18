package Chainsaw.crypto

import Chainsaw._
import Chainsaw.xilinx._

import scala.util.Random
import org.scalatest.flatspec.AnyFlatSpec

class ModularAddTest extends AnyFlatSpec {

  val testCount = 1000
  val M = Chainsaw.project.zprize.ZPrizeMSM.baseModulus

  def testModularAdd(): Unit = {
    val gen = ModularAdd(377, Some(M), BinaryAdder)
    val data = Seq.fill(testCount)(BigInt(377, Random)).filter(_ < M)
    "modular add" should "work" in ChainsawTest("testModularAdd", gen, data).doTest()
  }

  behavior of "modular adder"

  testModularAdd()

}
