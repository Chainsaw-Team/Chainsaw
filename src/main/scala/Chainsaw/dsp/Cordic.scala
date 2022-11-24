package Chainsaw.dsp

import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

sealed trait AlgebraicMode

object CIRCULAR extends AlgebraicMode

object HYPERBOLIC extends AlgebraicMode

object LINEAR extends AlgebraicMode

sealed trait RotationMode

object ROTATION extends RotationMode

object VECTORING extends RotationMode

import Chainsaw._
import NumericExt._

case class Cordic(algebraicMode: AlgebraicMode, rotationMode: RotationMode,
                  iteration: Int, fraction: Int)
  extends ChainsawGenerator {

  override def name = s"Cordic_${algebraicMode.getClass.getSimpleName}_${rotationMode.getClass.getSimpleName}_iter_${iteration}_fraction_$fraction".replace("$", "")

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[Double]]
    val Seq(x, y, z) = data
    algebraicMode match {
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
  }

  def cordicMetric(epsilonX: Double, epsilonY: Double, epsilonZ: Double) =
    (yours: Seq[Any], golden: Seq[Any]) => {
      val Seq(yourX, yourY, yourZ) = yours.asInstanceOf[Seq[Double]]
      val Seq(gX, gY, gZ) = golden.asInstanceOf[Seq[Double]]
      val xPass = (yourX - gX).abs <= epsilonX
      val yPass = (yourY - gY).abs <= epsilonY
      val zPass = (yourZ - gZ).abs <= epsilonZ
      xPass && yPass && zPass
    }

  override val metric = ChainsawMetric(frameWise = cordicMetric(1e-3, 1e-3, Pi / 180))

  override def generateTestCases: Seq[Double] = {
    def getGroup: Seq[Double] = {
      def getOne: Double = Random.nextDouble() * 2 - 1 // [-1,1]
      algebraicMode match {
        case CIRCULAR =>
          val phase0 = getOne * Pi // [-Pi, Pi]
          val length = getOne.abs
          val phase1 = 0.0
          Seq(cos(phase0) * length, sin(phase0) * length, phase1)
        case HYPERBOLIC => ???
        case LINEAR => ???
      }
    }
    Seq.fill(1000)(getGroup).flatten
  }

  val amplitudeType = SFixInfo(1, fraction)
  val phaseType = SFixInfo(2, fraction)

  override def inputTypes = Seq(amplitudeType, amplitudeType, phaseType)
  override def outputTypes = inputTypes

  override def inputFormat = inputNoControl
 override def outputFormat = inputNoControl
  override def latency = iteration + 1

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

  def getAmplitudeType(iter: Int) = {
    val minExp = -(fraction + iter)
    val lengthMax = sqrt(2) // (1,1)
    val scale = 1.0 / getScaleComplement(iter + 1)(algebraicMode) * lengthMax
    val maxExp = log(2.0, scale).ceil.toInt
    HardType(SFix(maxExp exp, minExp exp))
  }

  override def implH = new ChainsawModule(this) {

    implicit val mode: AlgebraicMode = algebraicMode

    val shiftingCoeffs = // shift value at each stage
      if (algebraicMode == HYPERBOLIC) getHyperbolicSequence(iteration)
      else 0 until iteration

    val scaleComplement = SF(getScaleComplement(iteration), 1 exp, -fraction exp)
    logger.info(s"compensation = ${getScaleComplement(iteration)}")

    def getCounterclockwise(group: Seq[SFix]) = rotationMode match {
      case ROTATION => ~group(2).asBits.msb // Z > 0
      case VECTORING => group(1).asBits.msb // Y < 0
    }

    val Seq(x, y, z) = sfixDataIn
    val determinant = (y.raw.msb ## x.raw.msb).asUInt

    /** --------
     * quadrant shift
     * -------- */
    val xyz = Vec(sfixDataIn.map(_.clone()))

    // TODO: extension for hyper and linear mode?
    algebraicMode match {
      case CIRCULAR =>
        switch(determinant) { // range extension
          is(U(1)) { // second quadrant
            val phaseShift = rotationMode match {
              case ROTATION => phaseType.fromConstant(Pi / 2)
              case VECTORING => -phaseType.fromConstant(Pi / 2)
            }
            xyz := Seq(y, -x, z + phaseShift)
          }
          is(U(3)) { // third quadrant
            val phaseShift = rotationMode match {
              case ROTATION => -phaseType.fromConstant(Pi / 2)
              case VECTORING => phaseType.fromConstant(Pi / 2)
            }
            xyz := Seq(-y, x, z + phaseShift)
          }

          default(xyz := sfixDataIn)
        }
      case HYPERBOLIC => ???
      case LINEAR => ???
    }

    /** --------
     * datapath
     * -------- */
    val ret = Seq.iterate((xyz, 0), iteration + 1) {
      case (Seq(x, y, z), i) =>
        val nextStageType = getAmplitudeType(i)
        assert(1.0 / getScaleComplement(i + 1) < nextStageType().maxValue)
        val shift = shiftingCoeffs(i)
        val phaseStep = phaseType.fromConstant(getPhaseCoeff(i))
        val xShifted = x >> shift
        val yShifted = y >> shift
        val direction = getCounterclockwise(Seq(x, y, z))
        // next-stage logic
        val xNext = (algebraicMode match {
          case CIRCULAR => Mux(direction, x - yShifted, x + yShifted)
          case HYPERBOLIC => Mux(direction, x + yShifted, x - yShifted)
          case LINEAR => x
        }).truncate(nextStageType).d()
        val yNext = Mux(direction, y + xShifted, y - xShifted).truncate(nextStageType).d()
        val zNext = Mux(direction, z - phaseStep, z + phaseStep).d()

        yNext.setName(s"y$i")

        //        logger.info(s"stage${i + 1}: x: ${xNext.maxExp}Q${-xNext.minExp}, y: ${yNext.maxExp}Q${-yNext.minExp}, z: ${zNext.maxExp}Q${-zNext.minExp}")
        (Vec(xNext, yNext, zNext), i + 1)
    }.last._1

    val Seq(xOut, yOut, zOut) = ret

    sfixDataOut(0) := (xOut * scaleComplement).d().truncated
    sfixDataOut(1) := (yOut * scaleComplement).d().truncated
    sfixDataOut(2) := zOut.d().truncated
  }
}

object Cordic {

  def phaseZero(fraction: Int) = SF(0, 2 exp, -fraction exp)

  def getAbsAndPhase(real: SFix, imag: SFix, iteration: Int, fraction: Int) = {
    val op = Cordic(CIRCULAR, VECTORING, iteration, fraction).asFunc
    val ret = op(Seq(real, imag, phaseZero(fraction)).map(_.asBits))
    (ret.head, ret.last) // (abs, phase)
  }
}