package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import Chainsaw.device.DSPMultFull
import spinal.core.IntToBuilder

import scala.language.postfixOps

case class KaraBase(widthA: Int, widthB: Int, multType: MultiplierType) extends ChainsawGenerator {

  require(Seq(widthA, widthB).sorted.zip(Seq(16, 25)).forall { case (yours, limit) => yours <= limit })

  override def name = s"karabase_${widthA}_${widthB}_type"

  if (multType != Kara) require(widthA == widthB, "rectangular result cannot be merged instantly!")

  val widthCross = widthA + widthB

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val Seq(aH, aL, bH, bL) = dataIn.asInstanceOf[Seq[BigInt]]

    val fullA = (aH << widthA) + aL
    val fullB = (bH << widthB) + bL
    val prod = fullA * fullB

    multType match {
      case FullMultiplier => Seq(prod)
      case SquareMultiplier => Seq(prod)
      case MsbMultiplier => Seq(prod.toBitValue(widthA * 2).takeHigh(widthA))
      case LsbMultiplier => Seq(prod.toBitValue(widthA * 2).takeLow(widthA))
      case Kara => Seq(aH * bH, aH * bL + aL * bH, aL * bL)
    }
  }

  override var inputTypes = Seq(widthA, widthA, widthB, widthB).map(UIntInfo(_))
  override var outputTypes = multType match {
    case FullMultiplier => Seq(UIntInfo(widthA * 4))      // should be 4
    case SquareMultiplier => Seq(UIntInfo(widthA * 4))    //
    case MsbMultiplier => Seq(UIntInfo(widthA * 2))
    case LsbMultiplier => Seq(UIntInfo(widthA * 2))
    case Kara => Seq(widthCross, widthCross + 1, widthCross).map(UIntInfo(_))
  }

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  override val outputTimes = Some(
    multType match {
      case Kara => Seq(0, 3, 0)
      case _ => Seq(0)
    }
  )

  override var latency = multType match {    // TODO
    case FullMultiplier => 6
    case SquareMultiplier => ???
    case MsbMultiplier => ???
    case LsbMultiplier => ???
    case Kara => 2
  }

  utilEstimation = VivadoUtilRequirement(dsp = 3)
  fmaxEstimation = 600 MHz

  def frameWiseMetric(yours: Seq[Any], golden: Seq[Any]) = {
    val y = yours.asInstanceOf[Seq[BigInt]]
    val g = golden.asInstanceOf[Seq[BigInt]]
    multType match {
      case MsbMultiplier => (y.head - g.head).abs <= 9
      case _ => y.equals(g)
    }
  }

  override val metric = ChainsawMetric(frameWise = frameWiseMetric)

  override def implH: ChainsawModule = new ChainsawModule(this) {

    multType match {
      case FullMultiplier =>
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
        val DSPMultFull = new DSPMultFull(widthA)
        DSPMultFull.io.a := aHigh.resize(widthA)
        DSPMultFull.io.b := aLow.resize(widthA)
        DSPMultFull.io.c := bHigh.resize(widthA)
        DSPMultFull.io.d := bLow.resize(widthA)
        uintDataOut := Seq(DSPMultFull.io.ret)

      case SquareMultiplier => ???    // TODO
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn

      case MsbMultiplier => ???    // TODO
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn

      case LsbMultiplier => ???    // TODO
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
        // 0-1
        val ALBH = (aLow.intoSInt * bHigh.intoSInt).d()
        val AHBL = (aHigh.intoSInt * bLow.intoSInt).d()
        val sumShift = (ALBH(widthA-1 downto 0) + AHBL(widthA-1 downto 0))
        val ALBL = (aLow.intoSInt * bLow.intoSInt).d()


      case Kara =>    // FIXME
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
        // 0-1
        val aMerge = (aHigh.intoSInt -^ aLow.intoSInt).d()
        val bMerge = (bHigh -^ bLow).asSInt.d() // outside
        // 0-2
        val high = (aHigh.intoSInt * bHigh.intoSInt).d(2) // dsp1
        val low = (aLow.intoSInt * bLow.intoSInt).d(2) // dsp2
        // 2-3, dsp0
        val mid0 = (aMerge.d() * bMerge.d()).d()
        // 3-4
        val mid1 = (high.d() - mid0).d() // 4-5 post-adder
        // 4-5
        val mid = (low.d(2) + mid1).d()
        uintDataOut := Seq(high.asUInt.resized, mid.asUInt.resized, low.asUInt.resized)
    }
  }
}
