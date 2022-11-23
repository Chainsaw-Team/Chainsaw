package Chainsaw.crypto

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FineReductionTest extends AnyFlatSpec {

  val testCount = 10000
  val M = Chainsaw.project.zprize.ZPrizeMSM.baseModulus

  def testFineReduction(upperBound: Int, lowerBound: Int): ChainsawTestReport = {
    val gen = FineReduction(M, upperBound, lowerBound)
    val data = Seq.fill(testCount)(BigInt(gen.widthIn + 1, Random) - (BigInt(1) << gen.widthIn)).filter(n => n < upperBound * M && n >= lowerBound * M)
    ChainsawTest("testFineReduction", gen, data).doTest()
  }

  "fine reduction module" should "work" in (-10 to 9).foreach(l => (l + 1 to 10).foreach(u => testFineReduction(u, l)))

  it should "synth" in ChainsawSynth(FineReduction(M, 10), "synthFineReduction")
}
