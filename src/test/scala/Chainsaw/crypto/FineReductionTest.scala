package Chainsaw.crypto

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FineReductionTest extends AnyFlatSpec {

  val testCount = 10000
  val upperBounds = 2 to 10
  val M = Chainsaw.project.zprize.ZPrizeMSM.baseModulus

  def testFineReduction(upperBound: Int): ChainsawTestReport = {
    val gen = FineReduction(M, upperBound)
    val data = Seq.fill(testCount)(BigInt(gen.widthIn, Random)).filter(_ < upperBound * M)
    ChainsawTest("testFineReduction", gen, data).doTest()
  }

  "fine reduction module" should "work" in upperBounds.foreach(testFineReduction)

  it should "synth" in ChainsawSynth(FineReduction(M, 10), "synthFineReduction")
}
