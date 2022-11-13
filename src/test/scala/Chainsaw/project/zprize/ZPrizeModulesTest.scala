package Chainsaw.project.zprize

import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw._
import Chainsaw.xilinx._
import ZPrizeModules._

import scala.util.Random

class ZPrizeModulesTest extends AnyFlatSpec {

  behavior of "BCMs"

  val dataWidth = 377
  val data = Seq.fill(1000)(BigInt(dataWidth, Random)) // compilation takes 2 minutes, 100000 takes 27 minutes, 1000 takes

  it should "work for ZPRIZE MSB mult" in ChainsawTest("test377Msb", msbMultGen, data).doTest()
  it should "work for ZPRIZE LSB mult" in ChainsawTest("test377Lsb", lsbMultGen, data).doTest()
}
