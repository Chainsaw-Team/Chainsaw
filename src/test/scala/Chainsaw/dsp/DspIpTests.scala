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
    val cordicFunctions = Seq(Rotate, Translate, SinAndCos, SinhAndCosh, ArcTan, ArcTanh, SquareRoot, Hypot, SquareDiffSqrt)
    for(archi <- 0 to 0){
      cordicFunctions.foreach(func =>
        testOperator(
          Cordic(
            iteration  = testIteration,
            fractional = testFraction,
            optimizeGoal = archi,
            errorEnable = false,
            functionSelect = func,
            bias = true
          ),
          generatorConfigTable("Cordic")
        )
      )
    }
  }

  /** -------- FIRs
    * --------
    */

  def testFirs(): Unit = {
    val coeffLengths = Seq(17, 24, 33)
    val coeffs       = coeffLengths.map(l => (1 to l).map(_ * 0.02))
    val symmetricCoeffs =
      coeffs.map(coeff => if (coeff.length % 2 == 0) coeff ++ coeff.reverse else coeff ++ coeff.init.reverse)



//    // Pipelined FIR
//    coeffs.foreach(coeff => testOperator(Fir(coeff, coeffType, dataType), generatorConfigTable("Fir")))
//    // Pipelined FIR with symmetric coefficients
//    symmetricCoeffs.foreach(coeff => testOperator(Fir(coeff, coeffType, dataType), generatorConfigTable("Fir")))

    // Parallel FIR by poly phase decomposition
    symmetricCoeffs.foreach(coeff =>
      testOperator(ParallelFir(coeff, coeffType, dataType, 2), generatorConfigTable("Fir"))
    )

    // TODO: test on higher parallel factor
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

//  def testFftAlgos() = {
//    CooleyTukeyFftAlgo(16).selfTest()
//  }

  def testShiftSpec(): Unit = {
    testOperator(
      ShiftSpec(250 MHz, 80 MHz, NumericType.SFix(1, 14), complex = true, 2),
      generatorConfigTable("ShiftSpec")
    )
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
      synth = false,
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
    ),
    "ShiftSpec" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    )
  )

//  testComplexMult()
testCordic()
//  testDelay()
//  testDds()
//  testMovingAverage()
//  testFirs()
//  testUnwrap()
//  testPeriodicUnwrap()
}
