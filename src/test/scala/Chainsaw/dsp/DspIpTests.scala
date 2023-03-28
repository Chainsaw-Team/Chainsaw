package Chainsaw.dsp

import Chainsaw._
import spinal.core.{IntToBuilder, _}

import scala.language.postfixOps // for more simulation

class DspIpTests extends ChainsawFlatSpec {

  /** -------- parameters
    * --------
    */
  val coeffType: NumericType = NumericType.SFix(2, 14)
  val dataType: NumericType  = NumericType.SFix(2, 14)
  val ddsType: NumericType   = NumericType(1, 16, signed = true)

  val testIteration = 12
  val testFraction  = 16

  val sizeMax   = 50
  val parallels = 1 to 4
  val dynamics  = Seq(true, false)

  val delay = 179

  /** -------- COMPLEX
    * --------
    */

  def testComplexMult(): Unit = testOperator(
    ComplexMult(dataType, dataType),
    generatorConfigTable("ComplexMult")
  )

  /** -------- CORDIC
    * --------
    */

  def testCordic(): Unit = {
    // CORDIC under all 6 modes
    val algebraicModes = Seq(CIRCULAR, HYPERBOLIC, LINEAR)
    val rotationModes  = Seq(ROTATION, VECTORING)

    algebraicModes.foreach(alg =>
      rotationModes.foreach(rot =>
        testOperator(
          Cordic(alg, rot, iteration = testIteration, fractional = testFraction),
          generatorConfigTable("Cordic")
        )
      )
    )

    // most frequently used CORDIC modes(with initValues)
    testOperator(
      CordicMagnitudePhase(
        iteration  = testIteration,
        fractional = testFraction
      ),
      generatorConfigTable("Cordic")
    )
    testOperator(
      CordicCosSin(iteration = testIteration, fractional = testFraction),
      generatorConfigTable("Cordic")
    )
    testOperator(
      CordicMultiplication(iteration = testIteration, fractional = testFraction),
      generatorConfigTable("Cordic")
    )
    testOperator(
      CordicDivision(iteration = testIteration, fractional = testFraction),
      generatorConfigTable("Cordic")
    )
    testOperator(
      CordicHyperFunction(iteration = testIteration, fractional = testFraction),
      generatorConfigTable("Cordic")
    )
    testOperator(
      CordicRotate(iteration = testIteration, fractional = testFraction),
      generatorConfigTable("Cordic")
    )
  }

  /** -------- FIRs
    * --------
    */

  def testFirs(): Unit = {

    val coeffLengths = Seq(17, 25, 33)
    val coeffs = coeffLengths.map(
      designFilter(_, Seq(1.6 MHz), 240 MHz, "lowpass").map(_.toDouble)
    )
    val symmetricCoeffs = coeffs.map(coeff => coeff ++ coeff.reverse)

    val symmetrics      = Seq(true, false)
    val parallelFactors = Seq(4)

    // Pipelined FIR
    coeffs.foreach(coeff => testOperator(Fir(coeff, coeffType, dataType), generatorConfigTable("Fir")))
    // Pipelined FIR with symmetric coefficients
    symmetricCoeffs.foreach(coeff =>
      testOperator(
        Fir(coeff, coeffType, dataType, symmetric = true),
        generatorConfigTable("Fir")
      )
    )
    // Parallel FIR by poly phase decomposition
    coeffs.foreach(coeff =>
      parallelFactors.foreach { parallelFactor =>
        testOperator(
          ParallelFir(coeff, coeffType, dataType, parallelFactor),
          generatorConfigTable("Fir")
        )
      }
    )
  }

  /** -------- UNWRAP
    * --------
    */

  /** -------- DDS
    * --------
    */
  def testDds(): Unit = {
    testOperator(
      Dds(
        ddsWave  = DdsWave(SINE, 250 MHz, 80 MHz, 1, 0),
        dataType = ddsType,
        parallel = 2
      ),
      generatorConfigTable("Dds")
    )
    testOperator(
      Dds(
        ddsWave  = DdsWave(SINE, 250 MHz, 80 MHz, 1, 0, complex = true),
        dataType = ddsType,
        parallel = 2
      ),
      generatorConfigTable("Dds")
    )
  }

  def testDelay(): Unit = {
    testOperator(
      DynamicDelay(delay, dataType),
      generatorConfigTable("DynamicDelay")
    )
  }

  /** -------- MOVING AVERAGE
    * --------
    */
  def testMovingAverage(): Unit = {
    val sizes = Seq(20, 50, 100)
    sizes.foreach(size =>
      testOperator(
        DynamicMovingAverage(size, dataType),
        generatorConfigTable("MovingAverage")
      )
    )
  }

  def testUnwrap() = {
    testOperator(
      UnwrapPointByPoint(NumericType.SFix(10, 14)),
      generatorConfigTable("Unwrap")
    )
  }

  def testPeriodicUnwrap() = {
    testOperator(
      PeriodicUnwrap(NumericType.SFix(10, 14), 20),
      generatorConfigTable("Unwrap")
    )
  }

  def testFftAlgos() = {
    CooleyTukeyFftAlgo(16).selfTest()
  }

  override def algoNames = Seq("CooleyTukeyFft")

  override def generatorConfigTable: Map[String, TestConfig] = Map(
    "ComplexMult" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    ),
    "Cordic" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    ),
    "DynamicDelay" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    ),
    "Fir" -> TestConfig(full = true, naive = false, synth = true, impl = false),
    "Dds" -> TestConfig(full = true, naive = false, synth = true, impl = false),
    "MovingAverage" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    ),
    "Unwrap" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    )
  )

//  testComplexMult()
//  testCordic()
  testDelay()
//  testDds()
//  testMovingAverage()
//  testFirs()
//  testUnwrap()
//  testPeriodicUnwrap()
//  testFftAlgos()

}
