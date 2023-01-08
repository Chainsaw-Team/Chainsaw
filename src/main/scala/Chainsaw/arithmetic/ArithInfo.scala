package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._

import scala.util.Random

/** describe the attributes of an UInt Operand needed for merge arithmetic
  *
  * @param width
  *   width of the operand
  * @param weight
  *   weight of the operand
  * @param isPositive
  *   signedness of the operand
  * @param time
  *   delay of the operand
  */
case class ArithInfo(
    width: Int,
    weight: Int,
    isPositive: Boolean = true,
    time: Int           = 0
) {

  def isNegative = !isPositive

  require(weight >= 0, "weight must be non-negative")

  def low = weight

  // TODO: high = low + width may be more reasonable
  def high = low + width - 1

  def range = high downto low

  def maxValue = if (isPositive) (pow2(width) - 1) << weight else BigInt(0)

  def <<(shiftLeft: Int) = ArithInfo(width, weight + shiftLeft, isPositive)

  def >>(shiftRight: Int): ArithInfo = <<(-shiftRight)

  def unary_- = ArithInfo(width, weight, !isPositive)

  def eval(value: BigInt) = {
    assert(
      value.bitLength <= width,
      s"yours: ${value.bitLength}, upper: $width"
    )
    (value << weight) * (if (isPositive) 1 else -1)
  }

  def withCarry(carry: Int) = ArithInfo(width + carry, weight, isPositive, time)

  def withTime(newTime: Int) = ArithInfo(width, weight, isPositive, newTime)

  def toPositive = ArithInfo(width, weight, isPositive = true, time)

  /** -------- FIXME: following methods are for Bm only
    * --------
    */
  def splitN(n: Int) = {
    val segmentWidth = width.divideAndCeil(n)
    (0 until n).map(i =>
      ArithInfo(
        width  = segmentWidth,
        weight = weight + i * segmentWidth,
        isPositive,
        time
      )
    )
  }

  def splitMsb = {
    require(isPositive)
    (
      ArithInfo(width = 1, weight, isPositive         = true, time),
      ArithInfo(width = width - 1, weight, isPositive = true, time)
    )
  }

  def +(that: ArithInfo) = {
    require(this.width == that.width)
    ArithInfo(
      width + 1,
      weight,
      isPositive,
      time + width.divideAndCeil(cpaWidthMax)
    )
  }

  def *(that: ArithInfo) = {
    ArithInfo(
      this.width + that.width,
      weight + that.weight,
      isPositive,
      time + 2
    )
  }

  def &(that: ArithInfo) = {
    require(that.width == 1)
    ArithInfo(width, weight + that.weight, isPositive, time)
  }

  def mergeWith(tail: Seq[ArithInfo], widthOut: Int) = {
    val all  = this +: tail
    val base = all.map(_.weight).min
    ArithInfo(widthOut, base, isPositive = true, all.map(_.time).max)
  }

  override def toString =
    s"${if (isPositive) "positive" else "negative"} $width-bit<<$weight at $time"
}

/** BigInt with ArithInfo, used for simulating UInt arithmetic
  */
case class WeightedBigInt(value: BigInt, arithInfo: ArithInfo) {
  require(
    value.bitLength <= arithInfo.width,
    s"the value width ${value.bitLength} is too large for the target width ${arithInfo.width}"
  )

  def eval: BigInt = arithInfo.eval(value)

  def <<(shiftLeft: Int) = WeightedBigInt(value, arithInfo << shiftLeft)

  def unary_- = WeightedBigInt(value, -arithInfo)

  def withWeight(weight: Int) = WeightedBigInt(
    value,
    ArithInfo(arithInfo.width, weight, arithInfo.isPositive, arithInfo.time)
  )

}

case class WeightedUInt(value: UInt, arithInfo: ArithInfo) {
  require(
    value.getBitsWidth <= arithInfo.width,
    s"the value width ${value.getBitsWidth} is too large for the target width ${arithInfo.width}"
  )

  def <<(shiftLeft: Int) = WeightedUInt(value, arithInfo << shiftLeft)

  def unary_- = WeightedUInt(value, -arithInfo)

  def withWeight(weight: Int) = WeightedUInt(
    value,
    ArithInfo(arithInfo.width, weight, arithInfo.isPositive, arithInfo.time)
  )
}

object ArithInfoGenerator {

  /** -------- random bit heap generators
    * --------
    */
  def genNoise(bound: Int): Int =
    Random.nextInt(2 * bound + 1) - bound // -bound ~ bound

  def genRectangularInfos(
      width: Int,
      height: Int,
      shift: Int                     = 0,
      sign: Boolean                  = true,
      withNoise: Boolean             = false,
      timeStrategy: TimeDiffStrategy = NoneTimeDiff,
      upBound: Int                   = 0
  ): Seq[ArithInfo] = {
    def randomShift: Int = if (withNoise) Random.nextInt(width) else 0

    val delta = if (height != 1) upBound / (height - 1) else upBound
    Seq.tabulate(height) { i =>
      val noise = randomShift
      timeStrategy match {
        case NoneTimeDiff => ArithInfo(width - noise, shift + noise, sign)
        case IncreaseTimeDiff =>
          ArithInfo(width - noise, shift + noise, sign, i * delta)
        case DecreaseTimeDiff =>
          ArithInfo(width - noise, shift + noise, sign, (height - i) * delta)
        case RandomTimeDiff =>
          ArithInfo(
            width - noise,
            shift + noise,
            sign,
            Random.nextInt(upBound + 1)
          )
      }
    }
  }

  // input bits formed a "triangle", typically generated by a multiplier
  def genTriangleInfos(
      width: Int,
      stairShape: (Int, Int)         = (1, 1),
      withNoise: Boolean             = false,
      truncate: Range                = null,
      sign: Boolean                  = true,
      timeStrategy: TimeDiffStrategy = NoneTimeDiff,
      upBound: Int                   = 0
  ): Seq[ArithInfo] = {
    require(
      (width / stairShape._1) % 2 == 1,
      s"the width / stairShape._1 must be odd number ! your input width is $width\t row stairShape is ${stairShape._1}"
    )
    if (truncate != null) {
      require(
        truncate.head >= 0 && truncate.last <= width - 1,
        s"the truncate range is out of width! your truncate start : ${truncate.head}  end : ${truncate.last}"
      )
    }
    val delta =
      if (width / stairShape._1 != 1) upBound / (width / stairShape._1 - 1)
      else upBound
    val infos = (0 until width / stairShape._1).flatMap { i =>
      val mid = ((width / stairShape._1) + 1) / 2
      val shift = (if (withNoise && i > 0) genNoise(stairShape._1)
                   else if (withNoise && i == 0) genNoise(stairShape._1).abs
                   else 0) + i * stairShape._1
      val number = mid - (i - mid + 1).abs
      timeStrategy match {
        case NoneTimeDiff =>
          Seq.fill(number * stairShape._2)(
            ArithInfo(
              (if (withNoise) genNoise(stairShape._1 / 5)
               else 0) + stairShape._1,
              shift,
              sign
            )
          )
        case IncreaseTimeDiff =>
          Seq.fill(number * stairShape._2)(
            ArithInfo(
              (if (withNoise) genNoise(stairShape._1 / 5)
               else 0) + stairShape._1,
              shift,
              sign,
              i * delta
            )
          )
        case DecreaseTimeDiff =>
          Seq.fill(number * stairShape._2)(
            ArithInfo(
              (if (withNoise) genNoise(stairShape._1 / 5)
               else 0) + stairShape._1,
              shift,
              sign,
              (width / stairShape._1 - i) * delta
            )
          )
        case RandomTimeDiff =>
          Seq.fill(number * stairShape._2)(
            ArithInfo(
              (if (withNoise) genNoise(stairShape._1 / 5)
               else 0) + stairShape._1,
              shift,
              sign,
              Random.nextInt(upBound + 1)
            )
          )
      }
    }

    if (truncate != null)
      infos
        .filter(info => info.high >= truncate.head && info.low <= truncate.last)
        .map { info =>
          var newLow  = info.low
          var newHigh = info.high
          if (info.low < truncate.head) newLow   = truncate.head
          if (info.high > truncate.last) newHigh = truncate.last
          ArithInfo(newHigh - newLow + 1, newLow, info.isPositive, info.time)
        }
    else infos
  }

  /** -------- graph generation utils
    * --------
    */

  abstract class InfosShape {
    def getConfig: Seq[(String, Any)]
  }

  case class Rectangle(
      width: Int,
      height: Int,
      shift: Int,
      sign: Boolean,
      withNoise: Boolean,
      mixSign: Boolean,
      timeStrategy: TimeDiffStrategy,
      upBound: Int
  ) extends InfosShape {
    override def toString =
      s"Rectangular : width:$width height:$height shift:$shift sign:$sign withNoise:$withNoise mixSign:$mixSign timeStrategy:${timeStrategy.getClass.getSimpleName.init} upBound:$upBound"

    override def getConfig = Seq(
      ("width", width),
      ("height", height),
      ("shift", shift),
      ("sign", sign),
      ("withNoise", withNoise),
      ("mixSign", mixSign),
      ("timeStrategy", timeStrategy),
      ("upBound", upBound)
    )
  }

  case class Triangle(
      width: Int,
      stairShape: (Int, Int),
      sign: Boolean,
      mixSign: Boolean,
      withNoise: Boolean,
      truncate: Range,
      timeStrategy: Strategy,
      upBound: Int
  ) extends InfosShape {
    override def toString =
      s"Triangle : width:$width stairShape:$stairShape sign:$sign mixSign:$mixSign withNoise:$withNoise " +
        s"truncate:${if (truncate == null) "All"
        else s"${truncate.head} to ${truncate.last}"} timeStrategy:${timeStrategy.getClass.getSimpleName.init} upBound:$upBound"

    override def getConfig = Seq(
      ("width", width),
      ("stairRowShape", stairShape._1),
      ("stairColShape", stairShape._2),
      ("withNoise", withNoise),
      ("truncate", truncate),
      ("timeStrategy", timeStrategy),
      ("upBound", upBound)
    )
  }

  object RectangularInfos {
    def apply(
        widthRange: Range              = Range.inclusive(128, 256, 32),
        heightRange: Range             = Range.inclusive(128, 256, 32),
        shift: Int                     = 0,
        sign: Boolean                  = true,
        withNoise: Boolean             = false,
        mixSign: Boolean               = false,
        timeStrategy: TimeDiffStrategy = NoneTimeDiff,
        upBound: Int                   = 0
    ): Seq[(Seq[ArithInfo], InfosShape)] =
      widthRange.flatMap { w =>
        heightRange.map { h =>
          (
            genRectangularInfos(
              w,
              h - (if (mixSign) h / (Random.nextInt(3) + 2) else 0),
              shift,
              sign,
              withNoise,
              timeStrategy,
              upBound
            )
              ++ genRectangularInfos(
                w,
                if (mixSign) h / (Random.nextInt(3) + 2) else 0,
                shift,
                !sign,
                withNoise,
                timeStrategy,
                upBound
              ),
            Rectangle(
              width        = w,
              height       = h,
              shift        = shift,
              sign         = sign,
              withNoise    = withNoise,
              mixSign      = mixSign,
              timeStrategy = timeStrategy,
              upBound      = upBound
            )
          )
        }
      }
  }

  object TriangleInfos {
    def apply(
        widthRange: Range              = Range.inclusive(255, 511, 32),
        stairRowShapeRange: Range      = Range.inclusive(1, 1),
        stairColShapeRange: Range      = Range.inclusive(1, 1),
        withNoise: Boolean             = false,
        truncate: Range                = null,
        sign: Boolean                  = true,
        mixSign: Boolean               = false,
        timeStrategy: TimeDiffStrategy = NoneTimeDiff,
        upBound: Int                   = 0
    ): Seq[(Seq[ArithInfo], InfosShape)] = widthRange.flatMap { w =>
      stairRowShapeRange.flatMap { r =>
        stairColShapeRange.map { c =>
          (
            genTriangleInfos(
              w,
              (r, c),
              withNoise,
              truncate,
              sign,
              timeStrategy = timeStrategy,
              upBound      = upBound
            )
              ++ (if (mixSign)
                    genTriangleInfos(
                      w,
                      (r, c),
                      withNoise,
                      truncate,
                      !sign,
                      timeStrategy = timeStrategy,
                      upBound      = upBound
                    )
                  else Seq[ArithInfo]()),
            Triangle(
              w,
              (r, c),
              sign,
              mixSign,
              withNoise,
              truncate,
              timeStrategy,
              upBound
            )
          )
        }
      }
    }
  }
}
