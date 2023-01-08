package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.device._
import Chainsaw.xilinx.{VivadoUtil, _}
import spinal.core._

import scala.language.postfixOps

/** efficient short multipliers, which are the building blocks of long
  * multipliers
  */
case class Sm(multiplierType: MultiplierType) extends UnsignedMultiplier {

  override def name = s"${className(multiplierType)}_32"

  override val widthX   = 32
  override val widthY   = 32
  override val widthOut = if (multiplierType == FullMultiplier) 64 else 32
  override val constant = None

  override def implH = new ChainsawOperatorModule(this) {

    val Seq(x, y) = dataIn
    val (xH, xL)  = x.asUInt().splitAt(16)
    val (yH, yL)  = y.asUInt().splitAt(16)

    multiplierType match {
      case FullMultiplier =>
        val core = new DSPMultFull(16)
        core.io.a    := xH.asUInt
        core.io.b    := xL.asUInt
        core.io.c    := yH.asUInt
        core.io.d    := yL.asUInt
        dataOut.head := core.io.ret.toAFix
      case MsbMultiplier =>
        val core = new DSPMultHigh(16)
        core.io.AH   := xH.asUInt
        core.io.AL   := xL.asUInt
        core.io.BH   := yH.asUInt
        core.io.BL   := yL.asUInt
        dataOut.head := core.io.ret.toAFix
      case LsbMultiplier =>
        val core = new DSPMultLow(16)
        core.io.AH   := xH.asUInt
        core.io.AL   := xL.asUInt
        core.io.BH   := yH.asUInt
        core.io.BL   := yL.asUInt
        dataOut.head := core.io.ret.toAFix
      // TODO: optimization for square multiplier
      case SquareMultiplier =>
    }
  }

  override def latency() = multiplierType match {
    case FullMultiplier   => 6
    case MsbMultiplier    => 7
    case LsbMultiplier    => 7
    case SquareMultiplier => ???
  }

  override def vivadoUtilEstimation = multiplierType match {
    case FullMultiplier   => VivadoUtil(dsp = 3, lut = 112)
    case MsbMultiplier    => VivadoUtil(dsp = 3)
    case LsbMultiplier    => VivadoUtil(dsp = 3)
    case SquareMultiplier => ???
  }

  override def fmaxEstimation = 800 MHz
}

/** efficients size implemented by Vivado + retiming, which consume no LUT at
  * all
  */
case class BaseDspMult(widthX: Int, widthY: Int) extends UnsignedMultiplier {

  override val constant       = None
  override val widthOut       = widthX + widthY
  override val multiplierType = FullMultiplier

  val level =
    if (widthX <= 17 && widthY <= 26) 1
    else if (widthX == widthY && widthX <= 26) 2
    else if (widthX <= 34 && widthY <= 34) 4
    else if (widthX <= 48 && widthY <= 48) 6
    else throw new IllegalArgumentException("unsupported size")

  override def implH = level match {
    case 6 =>
      new ChainsawOperatorModule(this) {
        // TODO: padding for width between 34 and 48
        val Seq(x, y) = dataIn.map(_.asUInt())

        val xWords = x.subdivideIn(16 bits)
        val yWords = y.subdivideIn(24 bits)

        val partials = xWords.zipWithIndex
          .map { case (xWord, i) =>
            val Seq(yLow, yHigh) = yWords
            val prodLow          = (xWord * yLow).d(2)
            val prodHigh         = (xWord.d() * yHigh.d()).d()
            val sum              = (prodLow.takeHigh(16).asUInt +^ prodHigh).d()
            val whole = sum @@ prodLow.takeLow(24).asUInt.d() // latency = 3
            whole << (i * 16)
          }
          .map(_.resize(96 bits))

        val ternaryGen = Compressor3to1(96)
        // TODO: replace ternary adder with merge operator
        dataOut.head := ternaryGen
          .process((partials ++ Seq.fill(2)(U(0, 1 bits))).map(_.toAFix))
          .head
          .d()
          .truncated
      }
    case _ => implNaiveH.get
  }

  override def latency() = level match {
    case 1 => 2
    case 2 => 5
    case 4 => 5
    case 6 => 4
  }

  override def name = s"BaseDspMult_${widthX}_$widthY"

  override def vivadoUtilEstimation = level match {
    case 1 => VivadoUtil(dsp = 1, lut = 0)
    case 2 => VivadoUtil(dsp = 2, lut = 0)
    case 4 => VivadoUtil(dsp = 4, lut = 0)
    case 6 => VivadoUtil(dsp = 6, lut = 96)
  }

  override def fmaxEstimation = level match {
    case 6 => 600 MHz
    case _ => 800 MHz
  }

  def prod(x: UInt, y: UInt) = {
    val core = implH
    core.dataIn.zip(Seq(x, y)).foreach { case (in, data) => in := data.toAFix }
    core.dataOut.head.asUInt()
  }
}
