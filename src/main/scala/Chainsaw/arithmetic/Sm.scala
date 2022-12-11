package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.device._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

/** efficient short multipliers, which are the building blocks of long multipliers
 *
 */
case class Sm(multiplierType: MultiplierType) extends UnsignedMultiplier {
  // currently, 32 X 32 multipliers only
  // TODO: implementation of 32 X 32 full multiplier with 4 DSP and no LUT consumption(as Vivado is able to do so)

  // TODO: implementation of 32 X 48 full multiplier with 4 DSP and as less LUTs as possible
  //    (at least <72, as flopoco is able to do so without taking advantage of pre-adder & post adder)
  //     if LUT consumption < 40, it will be a competitive building block for 377 X 377 long multiplier

  override def name = s"${className(multiplierType)}_32"

  override val widthX = 32
  override val widthY = 32
  override val widthOut = if (multiplierType == FullMultiplier) 64 else 32
  override val constant = None

  override def implH = new ChainsawOperatorModule(this) {

    val Seq(x, y) = dataIn
    val (xH, xL) = x.asUInt().splitAt(16)
    val (yH, yL) = y.asUInt().splitAt(16)

    multiplierType match {
      case FullMultiplier =>
        val core = new DSPMultFull(16)
        core.io.a := xH.asUInt
        core.io.b := xL.asUInt
        core.io.c := yH.asUInt
        core.io.d := yL.asUInt
        dataOut.head := core.io.ret.toAFix
      case MsbMultiplier =>
        val core = new DSPMultHigh(16)
        core.io.AH := xH.asUInt
        core.io.AL := xL.asUInt
        core.io.BH := yH.asUInt
        core.io.BL := yL.asUInt
        dataOut.head := core.io.ret.toAFix
      case LsbMultiplier =>
        val core = new DSPMultLow(16)
        core.io.AH := xH.asUInt
        core.io.AL := xL.asUInt
        core.io.BH := yH.asUInt
        core.io.BL := yL.asUInt
        dataOut.head := core.io.ret.toAFix
      // TODO: optimization for square multiplier
      case SquareMultiplier =>
    }
  }

  override def latency() = multiplierType match {
    case FullMultiplier => 6
    case MsbMultiplier => 7
    case LsbMultiplier => 7
    case SquareMultiplier => ???
  }

  override def vivadoUtilEstimation = multiplierType match {
    case FullMultiplier => VivadoUtilEstimation(dsp = 3, lut = 112)
    case MsbMultiplier => VivadoUtilEstimation(dsp = 3, lut = 0)
    case LsbMultiplier => VivadoUtilEstimation(dsp = 3, lut = 0)
    case SquareMultiplier => ???
  }

  override def fmaxEstimation = 800 MHz
}
