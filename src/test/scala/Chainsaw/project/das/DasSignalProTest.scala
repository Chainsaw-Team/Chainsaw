package Chainsaw.project.das

import Chainsaw.intel.QuartusFlow
import Chainsaw.{ChainsawFlatSpec, ChainsawTestWithData, TestCase}

class DasSignalProTest extends ChainsawFlatSpec {

  it should "work" in {

    val length = 31250

    val testCases = Seq(
      TestCase(rawData.flatten.take(length).map(BigDecimal(_)), Seq(BigDecimal(25))) // test with gaugeLength = 20m
    )

    val goldens = Seq(
      angleData.flatten.slice(31, length + 31).map(BigDecimal(_)).toSeq
    )

    ChainsawTestWithData(
      testName = "testDasSignalPro",
      gen = DasSignalPro(),
      data = testCases,
      golden = goldens
    )
  }

  it should "meet the fmax requirement" in new QuartusFlow(DasSignalPro().implH).impl()

}
