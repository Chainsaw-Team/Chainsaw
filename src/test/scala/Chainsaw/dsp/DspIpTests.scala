package Chainsaw.dsp

import Chainsaw._
import spinal.core.{False, IntToBuilder}

import scala.language.postfixOps
import scala.util.Random

class DspIpTests extends ChainsawFlatSpec {

  /** -------- parameters
    * --------
    */
  val coeffType = NumericType(2, 14, signed = true)
  val dataType  = NumericType(2, 14, signed = true)
  val ddsType   = NumericType(1, 16, signed = true)

  val testIteration = 12
  val testFraction  = 16

  val coeffLengths    = Seq(5, 11, 17)
  val coeffs          = coeffLengths.map(Seq.fill(_)(Random.nextDouble()))
  val symmetricCoeffs = coeffs.map(coeff => coeff ++ coeff.reverse)

  val symmetrics      = Seq(true, false)
  val parallelFactors = Seq(4)

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
    val algebraicModes = Seq(CIRCULAR)
    val rotationModes  = Seq(ROTATION, VECTORING)
    algebraicModes.foreach(alg =>
      rotationModes.foreach(rot =>
        testOperator(
          Cordic(
            alg,
            rot,
            iteration  = testIteration,
            fractional = testFraction
          ),
          generatorConfigTable("Cordic")
        )
      )
    )

    // most frequently used CORDIC modes(with initValues)
    testOperator(
      ComplexToMagnitudeAngle(
        iteration  = testIteration,
        fractional = testFraction
      ),
      generatorConfigTable("Cordic")
    )
    testOperator(
      CordicCos(iteration = testIteration, fractional = testFraction),
      generatorConfigTable("Cordic")
    )
    testOperator(
      CordicSin(iteration = testIteration, fractional = testFraction),
      generatorConfigTable("Cordic")
    )
  }

  /** -------- FIRs
    * --------
    */

  def testFirs(): Unit = {
    // Pipelined FIR
    coeffs.foreach(coeff =>
      testOperator(Fir(coeff, coeffType, dataType), generatorConfigTable("Fir"))
    )
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

  /** -------- MOVING AVERAGE
    * --------
    */
  //
  //  def testMovingAverage(): Unit = {
  //    dynamics.foreach { dynamic =>
  //      parallels.foreach { parallel =>
  //        testDspGenerator(MovingAverage(sizeMax, dynamic, dataType, parallel), synth = movingAverageSynth, impl = movingAverageImpl)
  //      }
  //    }
  //  }

  def testDelay(): Unit = {
    parallels.foreach { parallel =>
      if (parallel == 1)
        testOperator(
          DynamicDelay(delay, dataType, parallel),
          generatorConfigTable("DynamicDelay")
        )
    }
  }

  def testMovingAverage(): Unit = {
    val sizes = Seq(20, 50, 100)
    sizes.foreach(size =>
      testOperator(
        DynamicMovingAverage(size, dataType),
        generatorConfigTable("MovingAverage")
      )
    )
  }

  /** -------- tests
    * --------
    */

  override def generatorConfigTable = Map(
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
    )
  )

  testComplexMult()
  testCordic()
  testDelay()
  testFirs()
  testDds()
  testMovingAverage()
}
