import Chainsaw.{ChainsawDuts, ChainsawTest}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ChainsawTestTest extends AnyFlatSpec {

  behavior of "ChainsawTest"

  it should "pass for correct modules" in {

    (0 until 20).foreach { _ =>
      val gen = ChainsawDuts.simpleDut(correct = true)
      val test = ChainsawTest(
        testName = "simpleTest",
        gen = gen,
        data = Seq.fill(gen.inputFormat.rawDataCount * 5)(BigInt(4, Random))
      )
      test.doTest() // should be all true
    }
  }

  it should "not pass for wrong modules" in {
    val reports = (0 until 20).map { _ =>
      val gen = ChainsawDuts.simpleDut(correct = false)
      val test = ChainsawTest(
        testName = "simpleTest",
        gen = gen,
        data = Seq.fill(gen.inputFormat.rawDataCount * 5)(BigInt(4, Random)),
        silentTest = true
      )
      test.doTest()
    }
    assert(reports.forall(report => !report.passed)) // should be all false
  }
}