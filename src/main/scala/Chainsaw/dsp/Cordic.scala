package Chainsaw.dsp

import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.edaFlow.vivado.VivadoUtil
import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps
import scala.util.Random

sealed trait cordicFunction
object Rotate extends cordicFunction
object Translate extends cordicFunction
object SinAndCos extends cordicFunction
object SinhAndCosh extends cordicFunction
object ArcTan extends cordicFunction
object ArcTanh extends cordicFunction
object SquareRoot extends cordicFunction
object Hypot extends cordicFunction
object SquareDiffSqrt extends cordicFunction

/** Cordic Module Generator
  * @example
  *   Cordic(speedLevel, iteration, handshakeMode, fractional, optimizeGoal, functionSelect, errorEnable, bias)
  * @param speedLevel
  *   1 for 400MHz, 2 for 600 MHz on Xilinx UltraScale+
  * @param iteration
  *   User-defined iteration number
  * @param handshakeMode
  *   0 for Blocking, 1 for NonBlocking, Blocking has not yet been implemented
  * @param fractional
  *   User-defined fractional width
  * @param optimizeGoal
  *   0 for Serial architecture, 1 for unroll architecture
  * @param functionSelect
  *   Define 9 functions for user to choose
  * @param bias
  *   only valid on CIRCULAR-VECTORING and HYPERBOLIC-VECTORING
  * @see
  *   '''Xilinx FPGA Digital Signal Processing System Design Guide''' [[https://en.wikipedia.org/wiki/CORDIC]]
  * @see
  *   [[CordicModule]]
  */
case class Cordic(
    speedLevel: Int = 1,
    iteration: Int,
    handshakeMode: Int = 1, // 0:Blocking, 1:NonBlocking
    fractional: Int,
    optimizeGoal: Int, // 0:Area-Serial, 1:Throughput-unroll
    functionSelect: cordicFunction,
    bias: Boolean = false // only valid on CIRCULAR-VECTORING and HYPERBOLIC-VECTORING
) extends ChainsawOperatorGenerator
    with FixedLatency {

  require(Seq(1, 2).contains(speedLevel))

  val algebraicMode: AlgebraicMode = functionSelect match {
    case Rotate         => CIRCULAR
    case Translate      => CIRCULAR
    case SinAndCos      => CIRCULAR
    case SinhAndCosh    => HYPERBOLIC
    case ArcTan         => CIRCULAR
    case ArcTanh        => HYPERBOLIC
    case SquareRoot     => HYPERBOLIC
    case Hypot          => CIRCULAR
    case SquareDiffSqrt => HYPERBOLIC
  }

  val rotationMode: RotationMode = functionSelect match {
    case Rotate         => ROTATION
    case Translate      => VECTORING
    case SinAndCos      => ROTATION
    case SinhAndCosh    => ROTATION
    case ArcTan         => VECTORING
    case ArcTanh        => VECTORING
    case SquareRoot     => VECTORING
    case Hypot          => VECTORING
    case SquareDiffSqrt => VECTORING
  }

  val initValues: Seq[Option[Double]] = functionSelect match {
    case Rotate         => Seq(None, None, None)
    case Translate      => if (bias) Seq(None, None, None) else Seq(None, None, Some(0.0))
    case SinAndCos      => Seq(Some(1.0), Some(0.0), None)
    case SinhAndCosh    => Seq(Some(1.0), Some(0.0), None)
    case ArcTan         => if (bias) Seq(Some(1.0), None, None) else Seq(Some(1.0), None, Some(0.0))
    case ArcTanh        => if (bias) Seq(Some(1.0), None, None) else Seq(Some(1.0), None, Some(0.0))
    case SquareRoot     => Seq(None, Some(0.0), Some(0.0))
    case Hypot          => Seq(None, None, Some(0.0))
    case SquareDiffSqrt => Seq(None, None, Some(0.0))
  }

  val amplitudeType: NumericType = {
    if (functionSelect != SquareRoot) NumericType(1, fractional, true) // [-1, 1]
    else NumericType(1, fractional, false)                             // [0, 2)
  }
  val phaseType: NumericType = NumericType(2, fractional, true) // -Pi ~ Pi

  override def name = s"Cordic_" + s"${functionSelect}"

  logger.info(s"phaseType: $phaseType, amplitudeType: $amplitudeType")

  /** @param iteration
    *   the number of iterations of Cordic
    * @return
    *   iterate sequence in Hyperbolic mode
    */
  def getHyperbolicSequence(iteration: Int): Seq[Int] = {
    require(iteration < 54, "iteration times should be less than 54")
    val sequence = (1 to 4) ++ (4 to 13) ++ (13 to 40) ++ (40 to 50)
    sequence.slice(0, iteration)
  }

  /** @param iter
    *   iterate index
    * @param algebraicMode
    *   a specific mode in Cordic. e.g. CIRCULAR
    * @return
    *   phase offset for each iteration
    */
  def getPhaseCoeff(iter: Int)(implicit algebraicMode: AlgebraicMode) = {
    algebraicMode match {
      case CIRCULAR => atan(pow(2.0, -iter))
      case HYPERBOLIC =>
        atanh(
          pow(2.0, -getHyperbolicSequence(iter + 1).last)
        ) // for Hyperbolic mode, i begins with 1
      case LINEAR => pow(2.0, -(iter + 1)) // for LINEAR mode, i begins with 1
    }
  }

  /** @param iter
    *   total number of iterations
    * @param algebraicMode
    *   a specific mode in Cordic. e.g. CIRCULAR
    * @return
    *   amplitude compensation factor
    */
  def getScaleComplement(iter: Int)(implicit algebraicMode: AlgebraicMode) = {
    require(iter >= 1)
    algebraicMode match {
      case CIRCULAR => (0 until iter).map(i => cos(getPhaseCoeff(i))).product
      case HYPERBOLIC =>
        1.0 / (1 to iter)
          .map(i => getHyperbolicSequence(i).last)
          .map(i => sqrt(1 - pow(2.0, -2 * i)))
          .product
      case LINEAR => 1.0
    }
  }

  val xTolerance, yTolerance = 1e-2
  val zTolerance             = 1e-2 * 2 * Pi

  override def report: String =
    super.report + s"xTolerance: $xTolerance, yTolerance: $yTolerance, zTolerance: $zTolerance"

  override def implNaiveH = None

  /** quadrant transition latency in different mode
    */
  val quadrantShiftLatency = functionSelect match {
    case Rotate | SinAndCos | SquareDiffSqrt | Translate | Hypot | SquareRoot => speedLevel
    case ArcTan | SinhAndCosh | ArcTanh                                       => speedLevel - 1
  }

  /** iteration latency
    */
  val iterationLatency = speedLevel * iteration

  /** scale compensate latency
    */
  val scalingLatency = speedLevel

  /** @return
    *   total latency of Cordic
    */
  override def latency(): Int = quadrantShiftLatency + iterationLatency + scalingLatency

  /** Cordic algorithm prototype implementation
    */
  override def impl(testCase: TestCase): Seq[BigDecimal] = {
    val data    = testCase.data
    var current = 0
    val Seq(x, y, z) = initValues.map { init =>
      init.getOrElse {
        current += 1
        data(current - 1).toDouble
      }
    }

    val ret = functionSelect match {
      case Rotate         => Seq(x * cos(z) - y * sin(z), y * cos(z) + x * sin(z))
      case Translate      => Seq(sqrt(x * x + y * y), z + atan(y / x)) // get angle by atan2
      case SinAndCos      => Seq(cos(z), sin(z))
      case SinhAndCosh    => Seq(cosh(z), sinh(z))
      case ArcTan         => Seq(z + atan(y / x))
      case ArcTanh        => Seq(z + atanh(y / x))
      case SquareRoot     => Seq(sqrt(x))
      case Hypot          => Seq(sqrt(x * x + y * y))
      case SquareDiffSqrt => Seq(sqrt(x * x - y * y))
    }

    ret.map(BigDecimal(_))
  }

  /** define the error between software and hardware
    */
  // TODO:recode error informations
  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]): Boolean = {
    functionSelect match {
      case Rotate | SinAndCos | SinhAndCosh =>
        val Seq(x, y)   = yours
        val Seq(x1, y1) = golden
        amplitudeType.same(x, x1, xTolerance, 1e-2) && amplitudeType.same(y, y1, yTolerance, 1e-2)
      case Translate =>
        val Seq(x, z)   = yours
        val Seq(x1, z1) = golden
        amplitudeType.same(x, x1, xTolerance, 1e-2) && amplitudeType.same(z, z1, yTolerance, 1e-2)
      case ArcTan | ArcTanh =>
        val Seq(z)  = yours
        val Seq(z1) = golden
        amplitudeType.same(z, z1, xTolerance, 1e-2)
      case SquareRoot | Hypot | SquareDiffSqrt =>
        val Seq(x)  = yours
        val Seq(x1) = golden
        amplitudeType.same(x, x1, xTolerance, 1e-2)
    }
  }

  /** @return
    *   test cases
    */
  override def testCases: Seq[TestCase] = {
    def randRange(min: Double, max: Double): Double = min + (max - min) * Random.nextDouble() // [min, max)
    def randSign(): Int                             = if (Random.nextInt(2).toBoolean) 1 else -1
    def getGroup: Seq[BigDecimal] = (functionSelect match {
      case Rotate => Seq(randRange(-1.0, 1.0), randRange(-1.0, 1.0), randRange(-Pi, Pi))
      case Translate =>
        if (bias) Seq(randRange(-1.0, 1.0), randRange(-1.0, 1.0), randRange(-Pi / 2, Pi / 2))
        else Seq(randRange(-1.0, 1.0), randRange(-1.0, 1.0), 0.0)
      case SinAndCos   => Seq(1.0, 0.0, randRange(-Pi / 2, Pi / 2))
      case SinhAndCosh => Seq(1.0, 0.0, randRange(-1.1181, 1.1181))
      case ArcTan =>
        if (bias) Seq(1.0, randRange(-1.0, 1.0), randRange(-Pi / 2, Pi / 2)) else Seq(1.0, randRange(-1.0, 1.0), 0.0)
      case ArcTanh =>
        if (bias) Seq(1.0, randRange(-0.8069, 0.8069), randRange(-Pi / 2, Pi / 2))
        else Seq(1.0, randRange(-0.8069, 0.8069), 0.0)
      case SquareRoot => Seq(randRange(0.03, 1), 0.0, 0.0)
      case Hypot      => Seq(randRange(-1.0, 1.0), randRange(-1.0, 1.0), 0.0)
      case SquareDiffSqrt =>
        val x = randRange(-1.0, 1.0)
        val y = randRange(0, x.abs * 0.8069) * randSign()
        Seq(x, y, 0.0)
    }).zip(initValues).filter(_._2.isEmpty).map(_._1).map(BigDecimal(_))
    Seq.fill(10)(getGroup).map(TestCase(_))
  }

  override def inputTypes: Seq[NumericType] =
    Seq(amplitudeType, amplitudeType, phaseType)
      .zip(initValues)
      .flatMap {
        case (t, Some(_)) => None
        case (t, None)    => Some(t)
      }
  logger.info(s"inputTypes: ${inputTypes.mkString(", ")}")

  /** Rotate:output x, y
    *
    * Translate:output x, z
    *
    * SinAndCos:output x, y
    *
    * SinhAndCosh:output x, y
    *
    * ArcTan:output z
    *
    * ArcTanh:output z
    *
    * SquareRoot:output x
    *
    * Hypot:output x
    *
    * SquareDiffSqrt:output x
    */
  override def outputTypes = functionSelect match {
    case Rotate =>
      Seq(
        NumericType(1, fractional, true), // -sqrt(2) ~ sqrt(2)
        NumericType(1, fractional, true)  // -sqrt(2) ~ sqrt(2)
      )
    case Translate =>
      Seq(
        NumericType(1, fractional, false),         // [0, sqrt(2)]
        if (bias) NumericType(2, fractional, true) // [-Pi, Pi]
        else NumericType(1, fractional, true)      // [-Pi/2, Pi/2]
      )
    case SinAndCos =>
      Seq(
        NumericType(1, fractional, true), // [-1, 1]
        NumericType(1, fractional, true)  // [-1, 1]
      )
    case SinhAndCosh =>
      Seq(
        NumericType(1, fractional, false), // [0, 1.692968]
        NumericType(1, fractional, true)   // [-1.366, 1.366]
      )
    case ArcTan =>
      if (bias) Seq(NumericType(2, fractional, true)) // [-Pi, Pi]
      else Seq(NumericType(1, fractional, true))      // [-Pi/2, Pi/2]
    case ArcTanh =>
      if (bias) Seq(NumericType(2, fractional, true)) // [-Pi, Pi]
      else Seq(NumericType(1, fractional, true))      // [-Pi/2, Pi/2]
    case SquareRoot     => Seq(NumericType(0, fractional, false)) // [0, 1]
    case Hypot          => Seq(NumericType(1, fractional, false)) // [0, sqrt(2)]
    case SquareDiffSqrt => Seq(NumericType(1, fractional, false)) // [0, 1]
  }

  /** estimate cordic area consume(include lut, ff, dsp, bram36, uram288, carry8)
    */
  // TODO
  override def vivadoUtilEstimation = VivadoUtil(
    lut = algebraicMode match {
      case CIRCULAR   => 0
      case LINEAR     => 0
      case HYPERBOLIC => 0
    },
    ff = algebraicMode match {
      case CIRCULAR   => 0
      case LINEAR     => 0
      case HYPERBOLIC => 0
    },
    dsp     = 3,
    bram36  = 0,
    uram288 = 0,
    carry8  = 0
  )

  /** estimate max frequency under speed level(1 or 2)
    */
  // TODO: frequency at speedLevel=0, 1, 2...
  override def fmaxEstimation = if (speedLevel == 1) 400 MHz else 600 MHz

  /** -------- hardware implementation of the cordic
    * --------
    */
  override def implH = new CordicModule(this)

}

class CordicModule(cordic: Cordic) extends ChainsawOperatorModule(cordic) {

  import cordic._

  implicit val mode: AlgebraicMode = algebraicMode

  /** shift coeffcient in different mode
    */
  val shiftingCoeffs = {
    algebraicMode match {
      case CIRCULAR   => 0 until iteration
      case LINEAR     => 1 until iteration + 1
      case HYPERBOLIC => getHyperbolicSequence(iteration)
    }
  }

  /** compensation factor hardware type
    */
  // TODO
  val scaleType = NumericType(1, fractional, signed = false) // for 17 bit multiplier

  /** compensation factor
    */
  val scaleComplement =
    scaleType.fromConstant(getScaleComplement(iteration))
  logger.info(s"compensation = ${getScaleComplement(iteration)}")

  /** -------- getDataIn
    * --------
    */
  var current = 0
  val dataInUse = Seq(amplitudeType, amplitudeType, phaseType).zipWithIndex.map { case (numericType, i) =>
    if (initValues(i).isDefined) {
      numericType.fromConstant(initValues(i).get)
    } else {
      current += 1
      dataIn(current - 1)
    }
  }

  /** -------- quadrant shift
    * --------
    */
  val Seq(x, y, z) = dataInUse

  // input data assertion
  // when(validIn)(inputAssert(Seq(x, y, z)))

  val xyzPrev =
    if (functionSelect != SquareRoot) Vec(dataInUse.map(cloneOf(_)))
    else Vec(NumericType(1, fractional, false).apply(), NumericType(1, fractional, true).apply(), z.clone())

  // xyz: data type used in iteration
  val xyz = Seq(
    NumericType(2, fractional, true), // [-2, 2]
    NumericType(2, fractional, true), // [-2, 2]
    NumericType(2, fractional, true)  // [-Pi, Pi]
  ).map(_.apply())

  val quadrant = phaseType.fromConstant(Pi / 2)
  val quarter  = amplitudeType.fromConstant(0.25)

  // stage 0, add/sub
  // TODO: optimize determinantRot
  val determinantRot = ((z > quadrant) ## (z < quadrant.symmetricNegate)).asUInt.d()
  val determinantVec = (y.raw.msb ## x.raw.msb).asUInt.d()
  val xNegate        = if (functionSelect != SquareRoot) x.symmetricNegate.d() else x // else invalid
  val yNegate        = if (functionSelect != SquareRoot) y.symmetricNegate.d() else y // else invalid
  val zUp            = (z +| quadrant).d()
  val zDown          = (z -| quadrant).d()
  val xAddQuarter    = (x + quarter).d().fixTo(NumericType(1, fractional, false).apply())
  val xMinusQuarter  = (x - quarter).d().fixTo(NumericType(1, fractional, true).apply())

  // stage 1, mult
  functionSelect match {
    case Rotate | SinAndCos =>
      switch(determinantRot) {
        is(U(1))(xyzPrev := Seq(y.d(), xNegate, zUp))
        is(U(2))(xyzPrev := Seq(yNegate, x.d(), zDown))
        default(xyzPrev  := dataInUse.map(_.d()))
      }
    case Translate | Hypot =>
      switch(determinantVec) {
        is(U(1))(xyzPrev := Seq(y.d(), xNegate, zDown))
        is(U(3))(xyzPrev := Seq(yNegate, x.d(), zUp))
        default(xyzPrev  := dataInUse.map(_.d()))
      }
    case ArcTan | SinhAndCosh | ArcTanh => xyzPrev := dataInUse
    case SquareRoot                     => xyzPrev := Seq(xAddQuarter, xMinusQuarter, z.d())
    case SquareDiffSqrt =>
      xyzPrev                        := dataInUse.map(_.d())
      when(x.asBits.msb.d())(xyzPrev := Seq(xNegate, yNegate, z.d()))
  }

  xyz(0) := xyzPrev(0).fixTo(xyz(0)).d(speedLevel - 1)
  xyz(1) := xyzPrev(1).fixTo(xyz(1)).d(speedLevel - 1)
  xyz(2) := xyzPrev(2).fixTo(xyz(2)).d(speedLevel - 1)

  /** -------- iteration datapath, latency = iteration - 1
    * --------
    */
  def getCounterclockwise(group: Seq[AFix]) = rotationMode match {
    case ROTATION  => ~group(2).asBits.msb                      // Z > 0
    case VECTORING => group(0).asBits.msb ^ group(1).asBits.msb // d = sign(x,y)
  }

  val ret = optimizeGoal match {
    case 0 =>
      val start = {
        if (quadrantShiftLatency != 0) validIn.d(quadrantShiftLatency).simPublic() init False
        else validIn
      }
      val sel = Reg(Bool()) init False

      val controller = new Area {
        val fsm = new StateMachine {
          val IDLE    = makeInstantEntry() //avoid iteration not ending
          val WORKING = new State

          val counter = Counter(iteration, isActive(WORKING)) //when fsm in WORKING, counter+1
          IDLE.whenIsNext(counter.clear())

          IDLE.whenIsActive {
            goto(IDLE)
            when(start)(goto(WORKING))
          }
          WORKING.whenIsActive {
            goto(WORKING)
            when(counter.willOverflow)(goto(IDLE))
          }

          sel                      := False
          WORKING.whenIsActive(sel := True) //false: xyzTrans, true: xyzSel
        }
      }

      val dataPath = new Area {
        val Seq(xFirst, yFirst, zFirst) = Vec(xyz.map(cloneOf(_)))

        val xSel: AFix = Mux(sel, xFirst, xyz(0).d())
        val ySel: AFix = Mux(sel, yFirst, xyz(1).d())
        val zSel: AFix = Mux(sel, zFirst, xyz(2).d())

        val Seq(xShifteds, yShifteds) = functionSelect match {
          case Rotate | Translate | SinAndCos | ArcTan | Hypot =>
            Seq(
              for (i <- 0 until iteration) yield (xSel >> i).fixTo(xFirst),
              for (i <- 0 until iteration) yield (ySel >> i).fixTo(yFirst)
            )
          case SinhAndCosh | ArcTanh | SquareRoot | SquareDiffSqrt =>
            Seq(
              for (i <- getHyperbolicSequence(iteration)) yield (xSel >> i).fixTo(xFirst),
              for (i <- getHyperbolicSequence(iteration)) yield (ySel >> i).fixTo(yFirst)
            )
        }

        val phases = Mem((0 until iteration).map(i => phaseType.fromConstant(getPhaseCoeff(i)))) // constant value(rom)

        // stage 0, add/sub
        val xShifted  = xShifteds(controller.fsm.counter.value)
        val yShifted  = yShifteds(controller.fsm.counter.value)
        val phaseStep = phases.readAsync(controller.fsm.counter.value)

        val direction = getCounterclockwise(Seq(xSel, ySel, zSel)).d()
        val xUp       = (xSel +| yShifted).fixTo(xFirst).d()
        val xDown     = (xSel -| yShifted).fixTo(xFirst).d()
        val yUp       = (ySel +| xShifted).fixTo(yFirst).d()
        val yDown     = (ySel -| xShifted).fixTo(yFirst).d()
        val zUp       = (zSel +| phaseStep).fixTo(zFirst).d()
        val zDown     = (zSel -| phaseStep).fixTo(zFirst).d()

        // stage 1, mux
        val xNext = algebraicMode match {
          case CIRCULAR   => Mux(direction, xDown, xUp)
          case HYPERBOLIC => Mux(direction, xUp, xDown)
          case LINEAR     => xSel.d()
        }
        val yNext = Mux(direction, yUp, yDown)
        val zNext = Mux(direction, zDown, zUp)

        xFirst := xNext
        yFirst := yNext
        zFirst := zNext
      }
      Seq(dataPath.xFirst, dataPath.yFirst, dataPath.zFirst)

    case 1 =>
      // TODO: better width growth strategy
      Seq
        .iterate((xyz, 0), iteration + 1) { case (Seq(x, y, z), i) =>
          val shift     = shiftingCoeffs(i)
          val phaseStep = phaseType.fromConstant(getPhaseCoeff(i))
          // stage 0, add/sub
          val xShifted  = (x >> shift).fixTo(xyz(0))
          val yShifted  = (y >> shift).fixTo(xyz(1))
          val direction = getCounterclockwise(Seq(x, y, z)).d()
          val xUp       = (x +| yShifted).fixTo(xyz(0)).d()
          val xDown     = (x -| yShifted).fixTo(xyz(0)).d()
          val yUp       = (y +| xShifted).fixTo(xyz(1)).d()
          val yDown     = (y -| xShifted).fixTo(xyz(1)) d ()
          val zUp       = (z +| phaseStep).fixTo(xyz(2)).d()
          val zDown     = (z -| phaseStep).fixTo(xyz(2)).d()

          // stage 1, mux
          val xNext = (algebraicMode match {
            case CIRCULAR   => Mux(direction, xDown, xUp)
            case HYPERBOLIC => Mux(direction, xUp, xDown)
            case LINEAR     => x.d()
          }).d(speedLevel - 1)
          val yNext = Mux(direction, yUp, yDown).d(speedLevel - 1)
          val zNext = Mux(direction, zDown, zUp).d(speedLevel - 1)
          direction.setName(s"det_$i")
          xNext.setName(s"x_$i") // for better analysis
          yNext.setName(s"y_$i")
          zNext.setName(s"z_$i")
          (Vec(xNext, yNext, zNext), i + 1)
        }
        .last
        ._1
  }

  val Seq(xOut, yOut, zOut) = ret

  functionSelect match {
    case Rotate | SinAndCos | SinhAndCosh =>
      dataOut(0) := (xOut * scaleComplement).d(speedLevel).truncated
      dataOut(1) := (yOut * scaleComplement).d(speedLevel).truncated
      logger.info(s"output: x: ${dataOut(0).numericType}, y: ${dataOut(1).numericType}")
    case Translate =>
      dataOut(0) := (xOut * scaleComplement).d(speedLevel).truncated
      dataOut(1) := zOut.d(speedLevel).truncated
      logger.info(s"output: x: ${dataOut(0).numericType}, z: ${dataOut(1).numericType}")
    case ArcTan | ArcTanh =>
      dataOut(0) := zOut.d(speedLevel).truncated
      logger.info(s"output: z: ${dataOut(0).numericType}")
    case SquareRoot | Hypot | SquareDiffSqrt =>
      dataOut(0) := (xOut * scaleComplement).d(speedLevel).truncated
      logger.info(s"output: x: ${dataOut(0).numericType}")
  }
}
