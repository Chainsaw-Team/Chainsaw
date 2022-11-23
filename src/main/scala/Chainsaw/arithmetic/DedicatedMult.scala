package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.device.{DSPMultFull, DSPMultKara, DSPMultLow}
import Chainsaw.xilinx._
import spinal.core.{IntToBuilder, UInt}

import scala.language.postfixOps
import scala.util.Random

case class DedicatedMult(widthA: Int, widthB: Int, multType: MultiplierType, isKara: Boolean = false) extends ChainsawGenerator {

  /** --------
   * requirements
   * -------- */
  require(Seq(widthA, widthB).sorted.zip(Seq(16, 25)).forall { case (yours, limit) => yours <= limit }, s"your width: $widthA, $widthB, limit: 16, 25")
  val allowRectangular = multType == Kara || (multType == FullMultiplier && !isKara)
  if (!allowRectangular) require(widthA == widthB, "rectangular is not supported for current mode")

  override def name = s"karabase_${widthA}_${widthB}_type"

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

  def frameWiseMetric(yours: Seq[Any], golden: Seq[Any]): Boolean = {
    val y = yours.asInstanceOf[Seq[BigInt]]
    val g = golden.asInstanceOf[Seq[BigInt]]
    multType match {
      case MsbMultiplier =>
        val lowMax = (BigInt(1) << widthA) - 1
        val errorMax = lowMax * lowMax / (BigInt(1) << widthA) + 1
        val det = g.head - y.head <= errorMax
        if (!det) logger.info(s"errorMax: $errorMax, yours ${g.head - y.head}")
        det
      case _ => y.equals(g)
    }
  }

  override val metric = ChainsawMetric(frameWise = frameWiseMetric)

  override def generateTestCases = multType match {
    case SquareMultiplier => Seq.fill(1000)(Seq.fill(2)(BigInt(widthA, Random))).flatMap(seq => seq ++ seq)
    case _ => Seq.fill(1000)(Seq(BigInt(widthA, Random), BigInt(widthA, Random))).flatten
  }

  override var inputTypes = Seq(widthA, widthA, widthB, widthB).map(UIntInfo(_))
  override var outputTypes = multType match {
    case FullMultiplier => Seq(UIntInfo((widthA + widthB) * 2))
    case SquareMultiplier => Seq(UIntInfo(widthA * 4))
    case MsbMultiplier => Seq(UIntInfo(widthA * 2))
    case LsbMultiplier => Seq(UIntInfo(widthA * 2))
    case Kara => Seq(widthCross, widthCross + 1, widthCross).map(UIntInfo(_))
  }

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  override var latency = multType match {
    case FullMultiplier => 6
    case SquareMultiplier => 6
    case MsbMultiplier => 7
    case LsbMultiplier => 7
    case Kara => 5
  }

  utilEstimation = VivadoUtilRequirement(dsp = 3)
  fmaxEstimation = 600 MHz

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn

    multType match {
      case FullMultiplier => // Done
        val DSPMultFull = new DSPMultFull(widthA)
        DSPMultFull.io.a := aHigh.resize(widthA)
        DSPMultFull.io.b := aLow.resize(widthA)
        DSPMultFull.io.c := bHigh.resize(widthA)
        DSPMultFull.io.d := bLow.resize(widthA)
        uintDataOut := Seq(DSPMultFull.io.ret)

      case SquareMultiplier => // FIXME: currently this is the same as Full version
        val DSPMultFull = new DSPMultFull(widthA)
        DSPMultFull.io.a := aHigh.resize(widthA)
        DSPMultFull.io.b := aLow.resize(widthA)
        DSPMultFull.io.c := bHigh.resize(widthA)
        DSPMultFull.io.d := bLow.resize(widthA)
        uintDataOut := Seq(DSPMultFull.io.ret)

      case MsbMultiplier => ???

      case LsbMultiplier =>
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
        val DSPMultLow = new DSPMultLow(widthA)
        DSPMultLow.io.AH := aHigh
        DSPMultLow.io.AL := aLow
        DSPMultLow.io.BH := bHigh
        DSPMultLow.io.BL := bLow
        uintDataOut := Seq(DSPMultLow.io.ret)

      case Kara => // Done
        val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
        val DSPMultKara = new DSPMultKara(widthA, widthB)
        DSPMultKara.io.AH := aHigh
        DSPMultKara.io.AL := aLow
        DSPMultKara.io.BH := bHigh
        DSPMultKara.io.BL := bLow
        val high = DSPMultKara.io.AHBH
        val mid = DSPMultKara.io.AHBLALBH
        val low = DSPMultKara.io.ALBL
        uintDataOut := Seq(high.resized.d(2), mid.resized, low.resized.d())
    }
  }

  override def implNaiveH = Some(new ChainsawModule(this) {
    val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
    lazy val high = aHigh * bHigh
    lazy val low = aLow * bLow
    lazy val cross0 = aHigh * bLow
    lazy val cross1 = bHigh * aLow
    lazy val merge0 = aHigh +^ aLow
    lazy val merge1 = bHigh +^ bLow
    lazy val crossAll = (merge0 * merge1 - high - low).resize(widthCross + 1)

    val ret: Seq[UInt] = multType match {
      case Kara => Seq(high, crossAll, low)
      case _ =>
        val singleRet = multType match {
          case FullMultiplier =>
            if (isKara) (high << widthCross) + (crossAll << widthA) + low
            else (high << widthCross) + (cross0 << widthA) + (cross1 << widthB) + low
          case SquareMultiplier =>
            (high << widthCross) + (cross0 << (widthA + 1)) + low
          case MsbMultiplier =>
            ((high << widthA) + (cross0 +^ cross1)).takeHigh(widthA * 2).asUInt
          case LsbMultiplier =>
            (((cross0 +^ cross1) << widthA) + low).resize(widthA * 2) // resize = takeLow
        }
        Seq(singleRet)
    }
    uintDataOut := ret.map(_.d(latency))
  })
}

