package Chainsaw.arithmetic.bitheap

import Chainsaw.arithmetic._
import Chainsaw.{ChainsawFlatSpec, TestConfig, verbose}
import org.apache.commons.io.FileUtils

class BitHeapSolverTest extends ChainsawFlatSpec {

  /** --------
    *   1. testCases
    * --------
    */
  val testCases: Seq[Seq[ArithInfo]] = Seq(
    //    Seq.fill(9)(ArithInfo(200, 0)) ++ Seq.fill(9)(ArithInfo(200, 0, time = 1)), // positive, diff time
    //    Seq.fill(9)(ArithInfo(200, 0)) ++ Seq.fill(9)(ArithInfo(200, 0, isPositive = false)), // mixed, same time
    //    Seq.fill(9)(ArithInfo(200, 0)) ++ Seq.fill(9)(ArithInfo(200, 0, isPositive = false, time = 1)), // mixed, same time
    //    Seq.fill(5)(ArithInfo(100, 0)) ++ Seq.fill(5)(ArithInfo(100, 0, isPositive = false, time = 1)), // mixed, diff time
    //    Seq.fill(5)(ArithInfo(10, 2)) ++ Seq.fill(5)(ArithInfo(10, 1, isPositive = false, time = 1)), // mixed, diff time
    Seq(
      ArithInfo(64, 128),
      ArithInfo(64, 192),
      ArithInfo(64, 160),
      ArithInfo(64, 160, isPositive = false),
      ArithInfo(64, 160, isPositive = false),
      ArithInfo(32, 192),
      ArithInfo(32, 192),
      ArithInfo(1, 224)
    ) // case from Bm
  )

  /** -------- 2. solver tester function
    * --------
    */
  def testSolver(solverUnderTest: BitHeapSolver): Unit = {
    FileUtils.deleteDirectory(compressorSolutionOutputDir)
    compressorSolutionOutputDir.mkdir()
    testCases.foreach(testCase => {
      val gen = BitHeapCompressor(testCase, solverUnderTest)
      testOperator(gen, generatorConfigTable("BitHeapCompressor"))
    })
  }

  def testGreedSolver(): Unit = testSolver(GreedSolver)

  override def generatorConfigTable = Map(
    "BitHeapCompressor" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    )
  )

  testGreedSolver()
}
