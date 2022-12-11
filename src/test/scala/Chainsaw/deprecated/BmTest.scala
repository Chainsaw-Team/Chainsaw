package Chainsaw.deprecated

import Chainsaw._
import Chainsaw.arithmetic.{BmSolution, MultSearch}

import scala.util.Random

class BmTest extends ChainsawFlatSpec {

  behavior of "big multiplier"
    testFunction(96)
  

  val solution0 = MultSearch.getBmParetos(96, FullMultiplier).maxBy(_.vivadoUtil.dsp)
  testGenerator(Bm(96, None, solution0))

  def testFunction(width: Int): Unit = {
    val data = Seq.fill(10)(BigInt(width, Random))
    val squareData = data.flatMap(d => Seq(d, d))
    val solution0 = MultSearch.getBmParetos(width, FullMultiplier).maxBy(_.vivadoUtil.dsp)
    val solution1 = BmSolution(solution0.baseMultiplier, solution0.splits, solution0.multiplierType, true +: Seq.fill(solution0.length - 1)(false))
    val solution2 = MultSearch.getBmParetos(width, SquareMultiplier).minBy(_.vivadoUtil.dsp)

    it should s"work for low dsp $width" in {
      logger.info(solution0.toString)
      ChainsawTest("testBm", Bm(width, None, solution0), data).doTest()
    }

    it should s"work for schoolbook $width" in {
      logger.info(solution1.toString)
      ChainsawTest("testBm", Bm(width, None, solution1), data).doTest()
    }

    it should s"work for square $width" in {
      logger.info(solution2.toString)
      ChainsawTest("testBm", Bm(width, None, solution2), squareData).doTest()
    }
  }

  def testImplementation(width: Int): Unit = {
    val solution0 = MultSearch.getBmParetos(width, FullMultiplier).maxBy(_.vivadoUtil.dsp)
    val solution1 = BmSolution(solution0.baseMultiplier, solution0.splits, solution0.multiplierType, true +: Seq.fill(solution0.length - 1)(false))
    val solution2 = MultSearch.getBmParetos(width, SquareMultiplier).minBy(_.vivadoUtil.dsp)

    it should s"synth for high dsp $width" in {
      logger.info(solution0.toString)
      ChainsawSynth(Bm(width, None, solution0), s"synthKara$width")
    }

    it should s"synth for schoolbook $width" in {
      logger.info(solution1.toString)
      ChainsawSynth(Bm(width, None, solution1), s"synthKara$width")
    }

    it should s"synth for square $width" in {
      logger.info(solution2.toString)
      ChainsawSynth(Bm(width, None, solution1), s"synthKara$width")
    }
  }
}
