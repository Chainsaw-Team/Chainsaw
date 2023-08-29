package Chainsaw.dsp

import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.xilinx.VivadoUtil
import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

/** Cordic Module Generator
  * @example
  *   Cordic(HYPERBOLIC, ROTATION, iteration, fractional, Seq(Some(1.0), Some(0.0), None)), leave the third input as a
  *   port, bind th first and second input with constants
  * @param speedLevel
  *   1 for 400MHz, 2 for 600 MHz on Xilinx UltraScale+
  * @see
  *   '''Xilinx FPGA Digital Signal Processing System Design Guide''' [[https://en.wikipedia.org/wiki/CORDIC]]
  * @see
  *   [[CordicModule]]
  */
case class Cordic(
    algebraicMode: AlgebraicMode,
    rotationMode: RotationMode,
    iteration: Int,
    amplitudeType: NumericType,
    phaseFractional: Int,
    initValues: Seq[Option[Double]] = Seq(None, None, None),
    speedLevel: Int                 = 2
) extends ChainsawOperatorGenerator
    with FixedLatency {

  val phaseType = NumericType(Pi + 0.1, -Pi - 0.1, -phaseFractional)

  override def name = s"Cordic_" +
    s"${className(algebraicMode)}_${className(rotationMode)}_" +
    s"${iteration}_${amplitudeType}_${phaseType}_${initValues
      .map(value => if (value.isDefined) "1" else "0")
      .mkString("_")}"

  // methods getting coefficients
  def getHyperbolicSequence(iteration: Int): Seq[Int] = {
    require(iteration < 54, "iteration times should be less than 54")
    val sequence = (1 to 4) ++ (4 to 13) ++ (13 to 40) ++ (40 to 50)
    sequence.slice(0, iteration)
  }

  private def getHyperbolicIndex(iteration: Int) = getHyperbolicSequence(iteration).last

  def getPhaseCoeff(iter: Int)(implicit algebraicMode: AlgebraicMode) = {
    algebraicMode match {
      case CIRCULAR   => atan(pow(2.0, -iter))
      case HYPERBOLIC => atanh(pow(2.0, -getHyperbolicIndex(iter + 1)))
      case LINEAR     => pow(2.0, -iter)
    }
  }

  def getScaleComplement(iter: Int)(implicit algebraicMode: AlgebraicMode) = {
    require(iter >= 1)
    algebraicMode match {
      case CIRCULAR => (0 until iter).map(i => cos(getPhaseCoeff(i))).product // < 1.0
      case HYPERBOLIC =>
        val denominator = (1 to iter)
          .map(getHyperbolicIndex)
          .map(i => sqrt(1 - pow(2.0, -2 * i)))
          .product
        1.0 / denominator // > 1.0
      case LINEAR => 1.0
    }
  }

  val xTolerance, yTolerance = 1e-2
  val zTolerance             = 1e-2 * 2 * Pi

  override def report =
    super.report + s"xTolerance: $xTolerance, yTolerance: $yTolerance, zTolerance: $zTolerance"

  override def implNaiveH = None

  require(Seq(1, 2).contains(speedLevel), "speed level should be 1 or 2")

  // latency calculation
  val quadrantShiftLatency = algebraicMode match {
    case CIRCULAR =>
      rotationMode match {
        case ROTATION  => speedLevel
        case VECTORING => speedLevel
      }
    case LINEAR => speedLevel - 1
    case HYPERBOLIC =>
      rotationMode match {
        case ROTATION  => speedLevel - 1
        case VECTORING => speedLevel
      }
  }
  val iterationLatency = iteration * speedLevel
  val scalingLatency   = speedLevel

  override def latency() = quadrantShiftLatency + iterationLatency + scalingLatency

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase) = {
    val data    = testCase.data
    var current = 0
    val Seq(x, y, z) = initValues.map { init =>
      init.getOrElse {
        current += 1
        data(current - 1).toDouble
      }
    }

    val ret = algebraicMode match {
      case CIRCULAR =>
        rotationMode match {
          case ROTATION =>
            Seq(x * cos(z) - y * sin(z), y * cos(z) + x * sin(z), 0.0)
          case VECTORING =>
            Seq(sqrt(x * x + y * y), 0.0, z + atan2(y, x)) // get angle by atan2
        }
      case HYPERBOLIC =>
        rotationMode match {
          case ROTATION =>
            Seq(x * cosh(z) + y * sinh(z), y * cosh(z) + x * sinh(z), 0.0)
          case VECTORING =>
            Seq(sqrt(x * x - y * y), 0.0, z + atanh(y / x))
        }
      case LINEAR =>
        rotationMode match {
          case ROTATION  => Seq(x, y + x * z, 0.0)
          case VECTORING => Seq(x, 0.0, z + y / x)
        }
    }
    ret.map(BigDecimal(_))
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    val Seq(x, y, z)    = yours
    val Seq(x1, y1, z1) = golden
    amplitudeType.same(x, x1, xTolerance, 1e-2) &&
    amplitudeType.same(y, y1, yTolerance, 1e-2) &&
    phaseType.same(z, z1, zTolerance, 1e-2)
  }

  override def testCases: Seq[TestCase] = {
    // TODO: generate testCases according to amplitudeType
    def randRange(min: Double, max: Double): Double = min + (max - min) * Random.nextDouble() // [min, max)
    def randSign(): Int                             = if (Random.nextInt(2).toBoolean) 1 else -1
    def getOne: Double                              = Random.nextDouble() * 2 - 1             // [-1,1]
    def getGroup = (algebraicMode match {
      case CIRCULAR =>
        val phase0 = getOne * Pi              // [-Pi, Pi]
        val length = getOne.abs * 0.95 + 0.05 // avoid values too close to 0
        val phase1 = rotationMode match {
          case ROTATION  => getOne * Pi
          case VECTORING => 0
        }

        Seq(cos(phase0) * length, sin(phase0) * length, phase1)

      case LINEAR =>
        rotationMode match {
          case ROTATION =>
            val rotTestX = (Random.nextDouble() + 0.05) * randSign()
            val rotTestY = (Random.nextDouble() + 0.05) * randSign()
            val rotTestZ = (Random.nextDouble() * 1.85 + 0.05) * randSign() // |z| <= 2
            Seq(rotTestX, rotTestY, rotTestZ)
          case VECTORING =>
            val vecTestX = (Random.nextDouble() + 0.05) * randSign()
            val vecTestY = randRange(0, vecTestX.abs * 2) * randSign()
            val vecTestZ = getOne
            Seq(vecTestX, vecTestY, vecTestZ)
        }
      case HYPERBOLIC =>
        rotationMode match {
          case ROTATION => // |z| <= 1.1181
            val rotTestX = getOne
            val rotTestY = getOne
            val rotTestZ = getOne
            Seq(rotTestX, rotTestY, rotTestZ)
          case VECTORING => // |y| <= 0.8069|x|
            val vecTestX =
              (Random.nextDouble() + 0.05) * randSign() // plus 0.05 to avoid values too close to 0
            val vecTestY = randRange(0, vecTestX.abs * 0.8069) * randSign()
            val vecTestZ = getOne
            Seq(vecTestX, vecTestY, vecTestZ)
        }
    }).zip(initValues).filter(_._2.isEmpty).map(_._1).map(BigDecimal(_))

    Seq.fill(1000)(getGroup).map(TestCase(_))
  }
//
  override def inputTypes: Seq[NumericType] =
    Seq(amplitudeType, amplitudeType, phaseType)
      .zip(initValues)
      .flatMap {
        case (t, Some(_)) => None
        case (t, None)    => Some(t)
      }

  override def outputTypes = Seq(amplitudeType, amplitudeType, phaseType)

  override def vivadoUtilEstimation = VivadoUtil()

  override def fmaxEstimation = if (speedLevel == 1) 400 MHz else 600 MHz

  override def implH = new CordicModule(this)

}

class CordicModule(cordic: Cordic) extends ChainsawOperatorModule(cordic) {

  import cordic._

  implicit val mode: AlgebraicMode = algebraicMode

  private val shiftingCoeffs = // shift value at each stage
    if (algebraicMode == HYPERBOLIC) getHyperbolicSequence(iteration)
    else 0 until iteration

  private val scaleType       = NumericType.SFix(1, 16) // for 18 X n bit multiplier
  private val scaleComplement = scaleType.fromConstant(getScaleComplement(iteration))

  // get dataInUse according to initValues
  var current = 0
  private val dataInUse = Seq(amplitudeType, amplitudeType, phaseType).zipWithIndex
    .map { case (numericType, i) =>
      if (initValues(i).isDefined) numericType.fromConstant(initValues(i).get)
      else {
        current += 1
        dataIn(current - 1)
      }
    }

  // quadrant shift
  val Seq(x, y, z) = dataInUse
  val xyzPrev, xyz = Vec(dataInUse.map(cloneOf(_)))

  val quadrant = phaseType.fromConstant(Pi / 2)
  // stage 0, add/sub, preparing candidates
  val determinantVec = (y.raw.msb ## x.raw.msb).asUInt.d()
  val determinantRot = ((z > quadrant) ## (z < quadrant.symmetricNegate)).d().asUInt
  val yNegate        = y.symmetricNegate.d()
  val xNegate        = x.symmetricNegate.d()
  val zUp            = (z +| quadrant).d()
  val zDown          = (z -| quadrant).d()
  // stage 1, mux
  algebraicMode match {
    case CIRCULAR =>
      rotationMode match {
        case ROTATION =>
          switch(determinantRot) {
            is(U(1))(xyzPrev := Seq(y.d(), xNegate, zUp))   // 01 for z < -Pi/2
            is(U(2))(xyzPrev := Seq(yNegate, x.d(), zDown)) // 10 for z > Pi/2
            default(xyzPrev  := dataInUse.map(_.d()))
          }
        case VECTORING =>
          // for atan(rather than angle, switch Z values in second & third quadrant)
          switch(determinantVec) { // range extension
            is(U(1))(xyzPrev := Seq(y.d(), xNegate, zUp))   // second quadrant
            is(U(3))(xyzPrev := Seq(yNegate, x.d(), zDown)) // third quadrant
            default(xyzPrev  := dataInUse.map(_.d()))
          }
      }
    case HYPERBOLIC =>
      rotationMode match {
        case ROTATION => xyzPrev := dataInUse
        case VECTORING =>
          when(determinantVec.lsb)(xyzPrev := Seq(xNegate, yNegate, z.d())) // second/third quadrant
            .otherwise(xyzPrev := dataInUse.map(_.d()))
      }
    case LINEAR => xyzPrev := dataInUse // need not do anything
  }
  xyz := xyzPrev.d()

  /** -------- iteration datapath, latency = iteration - 1
    * --------
    */
  private def getCounterclockwise(group: Seq[AFix]) = rotationMode match {
    case ROTATION  => ~group(2).asBits.msb                      // Z > 0
    case VECTORING => group(0).asBits.msb ^ group(1).asBits.msb // d = sign(x,y)
  }

  val Seq(xOut, yOut, zOut) = Seq
    .iterate((xyz, 0), iteration + 1) { case (Seq(x, y, z), i) =>
      val shift     = shiftingCoeffs(i)
      val phaseStep = phaseType.fromConstant(getPhaseCoeff(i))
      // TODO: implement an iteration by addSub primitive with latency = 1?
      // stage 0, add/sub, preparing candidates
      val dataType = amplitudeType.withFractional(iteration) // running on an expanded bit width
      // TODO: better width growth strategy
      val direction = getCounterclockwise(Seq(x, y, z)).d() // determinant
      val xShifted  = (x >> shift).fixTo(dataType())
      val yShifted  = (y >> shift).fixTo(dataType())
      val xUp       = (x +| yShifted).d()
      val yUp       = (y +| xShifted).d()
      val xDown     = (x -| yShifted).d()
      val yDown     = (y -| xShifted).d()
      val zDown     = (z -| phaseStep).d()
      val zUp       = (z +| phaseStep).d()
      // stage 1, mux, selecting from candidates
      val xNext = (algebraicMode match {
        case CIRCULAR   => Mux(direction, xDown, xUp)
        case HYPERBOLIC => Mux(direction, xUp, xDown)
        case LINEAR     => x.d()
      }).d(speedLevel - 1)
      val yNext = Mux(direction, yUp, yDown).d(speedLevel - 1)
      val zNext = Mux(direction, zDown, zUp).d(speedLevel - 1)
      // for better analysis
      direction.setName(s"det_$i")
      xNext.setName(s"x_$i")
      yNext.setName(s"y_$i")
      zNext.setName(s"z_$i")
      (Vec(xNext, yNext, zNext), i + 1)
    }
    .last // final stage
    ._1   // xyz

  dataOut(0) := (xOut.fixTo(amplitudeType()) mult scaleComplement).d(speedLevel).truncated
  dataOut(1) := (yOut.fixTo(amplitudeType()) mult scaleComplement).d(speedLevel).truncated
  dataOut(2) := zOut.d(speedLevel)

  // for easy access
  val cosOut = dataOut(0)
  val sinOut = dataOut(1)

  val coshOut = dataOut(0)
  val sinhOut = dataOut(1)

  val amplitudeOut = dataOut(0)
  val phaseOut     = dataOut(2)
}

/** get magnitude and angle of (x,y)
  */
object CordicMagnitudePhase {
  def apply(iteration: Int, amplitudeType: NumericType, phaseFractional: Int): Cordic = {
    new Cordic(CIRCULAR, VECTORING, iteration, amplitudeType, phaseFractional, Seq(None, None, Some(0.0))) {
      override def name = s"ComplexToMagnitudeAngle_${iteration}_$phaseFractional"
    }
  }
}

/** rotate (x,y) counter-clockwise by z
  */
object CordicRotate {
  def apply(iteration: Int, amplitudeType: NumericType, phaseFractional: Int): Cordic = {
    new Cordic(CIRCULAR, ROTATION, iteration, amplitudeType, phaseFractional, Seq(None, None, None)) {
      override def name = s"CordicRotate_${iteration}_$phaseFractional"
    }
  }
}

/** get sin(z) and cos(z)
  */
object CordicCosSin {
  // first port is cos, second port is sin
  def apply(iteration: Int, amplitudeType: NumericType, phaseFractional: Int): Cordic = {
    new Cordic(CIRCULAR, ROTATION, iteration, amplitudeType, phaseFractional, Seq(Some(1.0), Some(0.0), None)) {
      override def name = s"CordicCosSin_${iteration}_$phaseFractional"
    }
  }
}

object CordicRealImag {
  // first port is real, second port is imag
  def apply(iteration: Int, amplitudeType: NumericType, phaseFractional: Int): Cordic = {
    new Cordic(CIRCULAR, ROTATION, iteration, amplitudeType, phaseFractional, Seq(None, Some(0.0), None)) {
      override def name = s"RealImag_${iteration}_$phaseFractional"
    }
  }
}

object CordicMultiplication {
  def apply(iteration: Int, amplitudeType: NumericType, phaseFractional: Int): Cordic = {
    new Cordic(LINEAR, ROTATION, iteration, amplitudeType, phaseFractional, Seq(None, Some(0.0), None)) {
      override def name = s"CordicMulti_${iteration}_$phaseFractional"
    }
  }
}

/** get x/y
  */
object CordicDivision {
  def apply(iteration: Int, amplitudeType: NumericType, phaseFractional: Int): Cordic = {
    new Cordic(LINEAR, VECTORING, iteration, amplitudeType, phaseFractional, Seq(None, None, Some(0.0))) {
      override def name = s"CordicDiv_${iteration}_$phaseFractional"
    }
  }
}

/** get sinh(z) and cosh(z)
  */
object CordicHyperFunction {
  def apply(iteration: Int, amplitudeType: NumericType, phaseFractional: Int): Cordic = {
    new Cordic(
      HYPERBOLIC,
      ROTATION,
      iteration,
      amplitudeType: NumericType,
      phaseFractional,
      Seq(Some(1.0), Some(0.0), None)
    ) {
      override def name = s"CordicHyperFun_${iteration}_$phaseFractional"
    }
  }
}
