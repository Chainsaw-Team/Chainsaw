package Chainsaw.arithmetic.bitheap

import Chainsaw.{ChainsawFlatSpec, TestConfig}
import Chainsaw.arithmetic._

class BitHeapSolverTest extends ChainsawFlatSpec {

  /** --------
    *   1. testCases
    * --------
    */
  val testCases: Seq[Seq[ArithInfo]] = Seq(
    Seq.fill(9)(ArithInfo(200, 0)) ++ Seq.fill(9)(ArithInfo(200, 0, time = 1)),                    // positive, diff time
    Seq.fill(9)(ArithInfo(200, 0)) ++ Seq.fill(9)(ArithInfo(200, 0, isPositive = false)),          // mixed, same time
    Seq.fill(9)(ArithInfo(200, 0)) ++ Seq.fill(9)(ArithInfo(200, 0, isPositive = false, time = 1)) // mixed, diff time
  )

  /** -------- 2. solver tester function
    * --------
    */
  def testNaiveSolver(): Unit = {
    testCases.foreach(testCase => {
      val gen = BitHeapCompressor(testCase, NaiveSolver)
      testOperator(gen, generatorConfigTable("BitHeapCompressor"))
    })
  }

  def testSolver(solverUnderTest: BitHeapSolver): Unit = {
    testCases.foreach(testCase => {
      val gen = BitHeapCompressor(testCase, solverUnderTest)
      testOperator(gen, generatorConfigTable("BitHeapCompressor"))
    })
  }

  def testGreedSolver() = testSolver(GreedSolver)

  override def generatorConfigTable = Map(
    "BitHeapCompressor" -> TestConfig(full = true, naive = false, synth = false, impl = false)
  )

  testNaiveSolver()
  testGreedSolver()

}
