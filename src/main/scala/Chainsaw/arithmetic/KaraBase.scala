package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import Chainsaw.device.{DSPMultFull, DSPMultLow, DSPMultHigh, DSPMultKara}
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
      case MsbMultiplier => Seq(prod.toBitValue(widthA * 4).takeHigh(widthA * 2))
      case LsbMultiplier => Seq(prod.toBitValue(widthA * 4).takeLow(widthA * 2))
      case Kara => Seq(aH * bH, aH * bL + aL * bH, aL * bL)
    }
  }

  override var inputTypes = Seq(widthA, widthA, widthB, widthB).map(UIntInfo(_))
  override var outputTypes = multType match {
    case FullMultiplier => Seq(UIntInfo(widthA * 4))
    case SquareMultiplier => Seq(UIntInfo(widthA * 4))
    case MsbMultiplier => Seq(UIntInfo(widthA * 2 + 1))    // TODO: to be checked
    case LsbMultiplier => Seq(UIntInfo(widthA * 2))
    case Kara => Seq(widthCross, widthCross + 1, widthCross).map(UIntInfo(_))
  }

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  override val outputTimes = Some(
    multType match {
      case Kara => Seq(0, 2, 1)    // old: Seq(0, 3, 0)
      case _ => Seq(0)
    }
  )

  override var latency = multType match {    // TODO
    case FullMultiplier => 6
    case SquareMultiplier => 6
    case MsbMultiplier => 7
    case LsbMultiplier => 7
    case Kara => 3    // old: 2
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
      case FullMultiplier =>    // Done
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
        val DSPMultFull = new DSPMultFull(widthA)
        DSPMultFull.io.a := aHigh.resize(widthA)
        DSPMultFull.io.b := aLow.resize(widthA)
        DSPMultFull.io.c := bHigh.resize(widthA)
        DSPMultFull.io.d := bLow.resize(widthA)
        uintDataOut := Seq(DSPMultFull.io.ret)

      case SquareMultiplier =>    // FIXME: 暂时用着dsp-full, 之后再修改成dsp-square
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
        val DSPMultFull = new DSPMultFull(widthA)
        DSPMultFull.io.a := aHigh.resize(widthA)
        DSPMultFull.io.b := aLow.resize(widthA)
        DSPMultFull.io.c := bHigh.resize(widthA)
        DSPMultFull.io.d := bLow.resize(widthA)
        uintDataOut := Seq(DSPMultFull.io.ret)

      case MsbMultiplier =>
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
        val DSPMultHigh = new DSPMultHigh(widthA)
        DSPMultHigh.io.AH := aHigh
        DSPMultHigh.io.AL := aLow
        DSPMultHigh.io.BH := bHigh
        DSPMultHigh.io.BL := bLow
        uintDataOut := Seq(DSPMultHigh.io.ret)

      case LsbMultiplier =>
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
        val DSPMultLow = new DSPMultLow(widthA)
        DSPMultLow.io.AH := aHigh
        DSPMultLow.io.AL := aLow
        DSPMultLow.io.BH := bHigh
        DSPMultLow.io.BL := bLow
        uintDataOut := Seq(DSPMultLow.io.ret)

      case Kara =>    // Done
          val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
          val DSPMultKara = new DSPMultKara(widthA, widthB)
          DSPMultKara.io.AH := aHigh
          DSPMultKara.io.AL := aLow
          DSPMultKara.io.BH := bHigh
          DSPMultKara.io.BL := bLow
          val high = DSPMultKara.io.AHBH
          val mid  = DSPMultKara.io.AHBLALBH
          val low  = DSPMultKara.io.ALBL
          uintDataOut := Seq(high.resized, mid.resized, low.resized)
    }
  }
}
