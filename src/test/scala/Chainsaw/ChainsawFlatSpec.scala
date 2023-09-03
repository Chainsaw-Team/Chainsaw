package Chainsaw

import Chainsaw.arithmetic.flopoco.FlopocoOperator
import org.scalatest.flatspec.AnyFlatSpec

import java.util.Calendar
import scala.util.Random

case class TestConfig(
    full: Boolean,
    naive: Boolean,
    synth: Boolean,
    impl: Boolean,
    naiveList: Seq[String]                           = Seq[String](),
    utilRequirementStrategy: UtilRequirementStrategy = DefaultRequirement
)

abstract class ChainsawFlatSpec extends AnyFlatSpec {

  val seed: Int = 42

  def generatorConfigTable: Map[String, TestConfig] = Map[String, TestConfig]()

  def algoNames: Seq[String] = Seq[String]()

  def showTable(map: Map[String, TestConfig]) = {
    val header    = s"| GeneratorName       | Full  | Naive | Synth | Impl  |"
    val separator = s"| ------------------- | ----- | ----- | ----- | ----- |"

    def getSymbol(boolean: Boolean) = s"   ${if (boolean) "●" else "○"}   "

    val body = map
      .map { case (name, config) =>
        val contents = Seq(config.full, config.naive, config.synth, config.impl)
          .map(getSymbol)
          .mkString("|")
        s"| ${name.padTo(20, " ").mkString("")}|$contents|"
      }
      .mkString("\n")
    s"$header\n$separator\n$body"
  }

  it should "show the test summary" in {
    Random.setSeed(seed)
    logger.info(s"scala random seed = $seed")
    logger.info(
      "------------Chainsaw test summary------------" +
        s"\nalgorithms: \n\t${algoNames.mkString("\n\t")}" +
        s"\ngenerators: " +
        s"\n${showTable(generatorConfigTable)}"
    )
  }

  def testOperator(
      gen: => ChainsawBaseGenerator,
      testConfig: TestConfig
  ): Unit = {

    import testConfig._

    behavior of gen.name

    if (full) {
      it should s"work correctly on testCase$testCaseIndex" in {
        ChainsawSimBox(testConfig.naiveList) {
          gen.doSelfTest()
        }
      }
    }

    if (naive) {
      it should s"has a correct naive implementation on testCase$testCaseIndex" in {
        gen.setAsNaive()
        assert(gen.useNaive)
        gen.doSelfTest()
        naiveSet.clear()
      }
    }

    if (synth && allowSynth) { // when impl is set, synth is not necessary
      it should s"meet the util & fmax requirement after synth on testCase$testCaseIndex" in
        ChainsawSynth(gen, testConfig.utilRequirementStrategy)
    }

    if (impl && allowImpl) {
      it should s"meet the util & fmax requirement after impl on testCase$testCaseIndex" in
        ChainsawImpl(gen, testConfig.utilRequirementStrategy)
    }
    testCaseIndex += 1
  }
}