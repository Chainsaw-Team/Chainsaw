package Chainsaw.memory

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._

import scala.util.Random

class DelayByRamTest extends AnyFlatSpec {

  "belay by Ram" should "work" in {
    val width = 64
    val delay = 128
    val data = Seq.fill(1000)(BigInt(width, Random))
    ChainsawTest("testDelayByram", DelayByRam(width, delay), data).doTest()
  }

  it should "run at a high fmax with a huge size" in ChainsawImpl(DelayByRam(512, 1024), "implDelayByRam")

}
