package Chainsaw.dsp

import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.xilinx.{VivadoUtilEstimation, VivadoUtilRequirement}
import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

case class Cordic(algebraicMode: AlgebraicMode,
                  rotationMode: RotationMode,
                  iteration: Int, fractional: Int,
                  initValues: Seq[Option[Double]] = Seq(None, None, None)
                 )
  extends ChainsawOperatorGenerator {

  override def name = s"Cordic_" +
    s"${className(algebraicMode)}_${className(rotationMode)}_" +
    s"${iteration}_${fractional}_${
      initValues.map(value => if (value.isDefined) "1" else "0").mkString("_")
    }"

  val phaseType = NumericTypeNew(Pi, -Pi, -fractional)
  val amplitudeType = NumericTypeNew(2.1, -2.1, -fractional)
  logger.info(s"phaseType: $phaseType, amplitudeType: $amplitudeType")

  /** --------
   * get coefficients
   * -------- */
  def getHyperbolicSequence(iteration: Int): Seq[Int] = {
    require(iteration < 54, "iteration times should be less than 54")
    val sequence = (1 to 4) ++ (4 to 13) ++ (13 to 40) ++ (40 to 50)
    sequence.slice(0, iteration)
  }

  def getPhaseCoeff(iter: Int)(implicit algebraicMode: AlgebraicMode) = {
    algebraicMode match {
      case CIRCULAR => atan(pow(2.0, -iter))
      case HYPERBOLIC => atanh(pow(2.0, -getHyperbolicSequence(iter + 1).last))
      case LINEAR => pow(2.0, -iter)
    }
  }

  def getScaleComplement(iter: Int)(implicit algebraicMode: AlgebraicMode) = {
    require(iter >= 1)
    algebraicMode match {
      case CIRCULAR => (0 until iter).map(i => cos(getPhaseCoeff(i))).product
      case HYPERBOLIC => 1.0 / (1 until iter)
        .map(i => getHyperbolicSequence(i).last)
        .map(i => sqrt(1 - pow(2.0, -2 * i))).product
      case LINEAR => 1.0
    }
  }

  val xTolerance, yTolerance = 1e-2
  val zTolerance = 1e-2 * 2 * Pi

  override def report = super.report + s"xTolerance: $xTolerance, yTolerance: $yTolerance, zTolerance: $zTolerance"

  override def implNaiveH = None

  override def latency() = iteration + 1

  /** --------
   * model
   * -------- */
  override def impl(testCase: TestCase) = {
    val data = testCase.data
    var current = 0
    val Seq(x, y, z) = initValues.map { init =>
      init.getOrElse {
        current += 1
        data(current - 1).toDouble
      }
    }

    val ret = algebraicMode match {
      case CIRCULAR => rotationMode match {
        case ROTATION => Seq(x * cos(z) - y * sin(z), y * cos(z) + x * sin(z), 0.0)
        case VECTORING => Seq(sqrt(x * x + y * y), 0.0, z + atan(y / x)) // atan is not the same as angle
      }
      case HYPERBOLIC => rotationMode match {
        case ROTATION => Seq(x * cosh(z) - y * sinh(z), y * cosh(z) + x * sinh(z), 0.0)
        case VECTORING => Seq(sqrt(x * x - y * y), 0.0, z + atanh(y / x))
      }
      case LINEAR => rotationMode match {
        case ROTATION => Seq(x, y + x * z, 0.0)
        case VECTORING => Seq(x, 0.0, z + y / x)
      }
    }
    ret.map(BigDecimal(_))
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    val Seq(x, y, z) = yours
    val Seq(x1, y1, z1) = golden
    amplitudeType.same(x, x1, xTolerance, 1e-2) &&
      amplitudeType.same(y, y1, yTolerance, 1e-2) &&
      phaseType.same(z, z1, zTolerance, 1e-2)
  }

  override def testCases = {
    def getOne: Double = Random.nextDouble() * 2 - 1 // [-1,1]

    def getGroup = (algebraicMode match {
      case CIRCULAR =>
        val phase0 = getOne * Pi // [-Pi, Pi]
        val length = getOne.abs
        val phase1 = 0.0
        Seq(cos(phase0) * length, sin(phase0) * length, phase1)
      case HYPERBOLIC => ???
      case LINEAR => ???
    }).zip(initValues).filter(_._2.isEmpty).map(_._1).map(BigDecimal(_))

    Seq.fill(1000)(getGroup).map(TestCase(_))
  }

  override def inputTypes: Seq[NumericTypeNew] =
    Seq(amplitudeType, amplitudeType, phaseType).zip(initValues)
      .flatMap {
        case (t, Some(_)) => None
        case (t, None) => Some(t)
      }

  logger.info(s"inputTypes: ${inputTypes.mkString(", ")}")

  override def outputTypes = Seq(amplitudeType, amplitudeType, phaseType)

  override def vivadoUtilEstimation = VivadoUtilEstimation()

  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawOperatorModule(this) {

    implicit val mode: AlgebraicMode = algebraicMode

    val shiftingCoeffs = // shift value at each stage
      if (algebraicMode == HYPERBOLIC) getHyperbolicSequence(iteration)
      else 0 until iteration

    val scaleType = NumericTypeNew(1, 16, signed = true) // for 18 bit multiplier
    val scaleComplement = scaleType.fromConstant(getScaleComplement(iteration))
    logger.info(s"compensation = ${getScaleComplement(iteration)}")

    def getCounterclockwise(group: Seq[AFix]) = rotationMode match {
      case ROTATION => ~group(2).asBits.msb // Z > 0
      case VECTORING => group(1).asBits.msb // Y < 0
    }

    var current = 0

    // get initial values according to initValues
    val dataInUse = Seq(amplitudeType, amplitudeType, phaseType).zipWithIndex
      .map { case (t, i) =>
        if (initValues(i).isDefined) t.fromConstant(initValues(i).get)
        else {
          current += 1
          dataIn(current - 1)
        }
      }
    val Seq(x, y, z) = dataInUse
    val determinant = (y.raw.msb ## x.raw.msb).asUInt

    /** --------
     * quadrant shift
     * -------- */
    val xyz = Vec(dataInUse.map(cloneOf(_)))

    // TODO: extension for hyper and linear mode?
    algebraicMode match {
      case CIRCULAR =>
        switch(determinant) { // range extension
          is(U(1)) { // second quadrant
            val phaseShift = rotationMode match {
              case ROTATION => phaseType.fromConstant(Pi / 2)
              case VECTORING => phaseType.fromConstant(Pi / 2).symmetricNegate
            }
            xyz := Seq(y, x.symmetricNegate, z +| phaseShift)
          }
          is(U(3)) { // third quadrant
            val phaseShift = rotationMode match {
              case ROTATION => phaseType.fromConstant(Pi / 2).symmetricNegate
              case VECTORING => phaseType.fromConstant(Pi / 2)
            }
            xyz := Seq(y.symmetricNegate, x, z +| phaseShift)
          }
          default(xyz := dataInUse)
        }
      case HYPERBOLIC => ???
      case LINEAR => ???
    }

    /** --------
     * datapath
     * -------- */
    // TODO: better width growth strategy
    val ret = Seq.iterate((xyz, 0), iteration + 1) {
      case (Seq(x, y, z), i) =>
        val shift = shiftingCoeffs(i)
        val phaseStep = phaseType.fromConstant(getPhaseCoeff(i))
        val xShifted = (x >> shift).fixTo(amplitudeType())
        val yShifted = (y >> shift).fixTo(amplitudeType())
        val direction = getCounterclockwise(Seq(x, y, z))
        // next-stage logic
        val xNext = (algebraicMode match {
          case CIRCULAR => Mux(direction, x -| yShifted, x +| yShifted)
          case HYPERBOLIC => Mux(direction, x +| yShifted, x -| yShifted)
          case LINEAR => x
        }).d()
        val yNext = Mux(direction, y +| xShifted, y -| xShifted).d()
        val zNext = Mux(direction, z -| phaseStep, z +| phaseStep).d()

        (Vec(xNext, yNext, zNext), i + 1)
    }.last._1

    val Seq(xOut, yOut, zOut) = ret
    logger.info(s"output: x: ${xOut.numericType}, y: ${yOut.numericType}, z: ${zOut.numericType}")

    dataOut(0) := (xOut * scaleComplement).d().truncated
    dataOut(1) := (yOut * scaleComplement).d().truncated
    dataOut(2) := zOut.d()

    def phaseOut = dataOut(2)

    def cosOut = dataOut(0)

    def sinOut = dataOut(0)
  }
}

object ComplexToMagnitudeAngle {
  def apply(iteration: Int, fractional: Int) = {
    new Cordic(CIRCULAR, VECTORING, iteration, fractional, Seq(None, None, Some(0.0))) {
      override def name = s"ComplexToMagnitudeAngle_${iteration}_$fractional"
    }
  }
}

object CordicCos {
  def apply(iteration: Int, fractional: Int) = {
    new Cordic(CIRCULAR, ROTATION, iteration, fractional, Seq(Some(1.0), Some(0.0), None)) {
      override def name = s"CordicCos_${iteration}_$fractional"
    }
  }
}

object CordicSin {
  def apply(iteration: Int, fractional: Int) = {
    new Cordic(CIRCULAR, ROTATION, iteration, fractional, Seq(Some(0.0), Some(1.0), None)) {
      override def name = s"CordicSin_${iteration}_$fractional"
    }
  }
}


