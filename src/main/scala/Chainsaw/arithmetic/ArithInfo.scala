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
      randomSign: Boolean            = false,
      withNoise: Boolean             = false,
      timeStrategy: TimeDiffStrategy = NoneTimeDiff,
      timeUpBound: Int               = 0
  ): Seq[ArithInfo] = {
    def randomNoise: Int = if (withNoise) Random.nextInt(width / 2) else 0

    val delta     = if (height != 1) timeUpBound / (height - 1) else timeUpBound
    val highNoise = randomNoise
    Seq.tabulate(height) { i =>
      val shiftNoise = randomNoise
      def randomHighNoise =
        if (withNoise) Random.nextInt(width / 2 - highNoise) else 0
      timeStrategy match {
        case NoneTimeDiff =>
          ArithInfo(
            width - shiftNoise - highNoise - randomHighNoise,
            shift + shiftNoise,
            if (randomSign) Random.nextBoolean() else sign,
            timeUpBound
          )
        case IncreaseTimeDiff =>
          ArithInfo(
            width - shiftNoise - highNoise - randomHighNoise,
            shift + shiftNoise,
            if (randomSign) Random.nextBoolean() else sign,
            i * delta
          )
        case DecreaseTimeDiff =>
          ArithInfo(
            width - shiftNoise - highNoise - randomHighNoise,
            shift + shiftNoise,
            if (randomSign) Random.nextBoolean() else sign,
            (height - i) * delta
          )
        case RandomTimeDiff =>
          ArithInfo(
            width - shiftNoise - highNoise - randomHighNoise,
            shift + shiftNoise,
            if (randomSign) Random.nextBoolean() else sign,
            Random.nextInt(timeUpBound + 1)
          )
      }
    }
  }

  // input bits formed a "triangle", typically generated by a multiplier
  def genTriangleInfos(
      width: Int,
      stairShape: (Int, Int)         = (1, 1),
      shift: Int                     = 0,
      withNoise: Boolean             = false,
      truncate: Range                = null,
      randomTruncate: Boolean        = false,
      sign: Boolean                  = true,
      randomSign: Boolean            = false,
      timeStrategy: TimeDiffStrategy = NoneTimeDiff,
      timeUpBound: Int               = 0
  ): Seq[ArithInfo] = {
    val (rowStairParameter, colStairParameter) = stairShape
    if ((width / rowStairParameter) % 2 == 1)
      logger.info(
        s"the width / rowStairParameter must be odd number ! your width is $width\t rowStairParameter is $rowStairParameter, now expand width to ${width + rowStairParameter}"
      )
    val fixedWidth =
      if ((width / rowStairParameter) % 2 == 1) width
      else width + rowStairParameter
    if (truncate != null && !randomTruncate) {
      logger.info(
        s"the truncate range is out of width! your truncate: [${truncate.head}:${truncate.last}], now adjust to [${truncate.head max 0}:${truncate.last min fixedWidth - 1}]",
        truncate.head >= 0 && truncate.last <= fixedWidth - 1
      )
    }
    val fixedTruncate = if (randomTruncate) {
      val randomValues =
        Seq(Random.nextInt(fixedWidth), Random.nextInt(fixedWidth))
      val (minRandomValue, maxRandomValue) =
        (randomValues.min, randomValues.max)
      Range.inclusive(minRandomValue, maxRandomValue)
    } else {
      if (truncate == null) truncate
      else
        Range.inclusive(truncate.head max 0, truncate.last min fixedWidth - 1)
    }

    val delta =
      if (fixedWidth / rowStairParameter != 1)
        timeUpBound / (fixedWidth / rowStairParameter - 1)
      else timeUpBound
    val infos = (0 until fixedWidth / rowStairParameter).flatMap { i =>
      val mid = ((fixedWidth / rowStairParameter) + 1) / 2
      val regShift = (if (withNoise && i > 0) genNoise(rowStairParameter)
                      else if (withNoise && i == 0)
                        genNoise(rowStairParameter).abs
                      else 0) + i * rowStairParameter
      val number = mid - (i - mid + 1).abs
      timeStrategy match {
        case NoneTimeDiff =>
          Seq.fill(
            number * colStairParameter - (if (withNoise)
                                            genNoise(rowStairParameter / 5)
                                          else 0)
          )(
            ArithInfo(
              rowStairParameter - (if (withNoise)
                                     genNoise(rowStairParameter / 5)
                                   else 0),
              shift + regShift,
              if (randomSign) Random.nextBoolean() else sign,
              timeUpBound
            )
          )
        case IncreaseTimeDiff =>
          Seq.fill(
            number * colStairParameter - (if (withNoise)
                                            genNoise(rowStairParameter / 5)
                                          else 0)
          )(
            ArithInfo(
              rowStairParameter - (if (withNoise)
                                     genNoise(rowStairParameter / 5)
                                   else 0),
              shift + regShift,
              if (randomSign) Random.nextBoolean() else sign,
              i * delta
            )
          )
        case DecreaseTimeDiff =>
          Seq.fill(
            number * colStairParameter - (if (withNoise)
                                            genNoise(rowStairParameter / 5)
                                          else 0)
          )(
            ArithInfo(
              rowStairParameter - (if (withNoise)
                                     genNoise(rowStairParameter / 5)
                                   else 0),
              shift + regShift,
              if (randomSign) Random.nextBoolean() else sign,
              (fixedWidth / rowStairParameter - i) * delta
            )
          )
        case RandomTimeDiff =>
          Seq.fill(
            number * colStairParameter - (if (withNoise)
                                            genNoise(rowStairParameter / 5)
                                          else 0)
          )(
            ArithInfo(
              rowStairParameter - (if (withNoise)
                                     genNoise(rowStairParameter / 5)
                                   else 0),
              shift + regShift,
              if (randomSign) Random.nextBoolean() else sign,
              Random.nextInt(timeUpBound + 1)
            )
          )
      }
    }

    if (fixedTruncate != null)
      infos
        .filter(info => info.high >= fixedTruncate.head && info.low <= fixedTruncate.last)
        .map { info =>
          var newLow  = info.low
          var newHigh = info.high
          if (info.low < fixedTruncate.head) newLow   = fixedTruncate.head
          if (info.high > fixedTruncate.last) newHigh = fixedTruncate.last
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
      randomSign: Boolean,
      withNoise: Boolean,
      timeStrategy: TimeDiffStrategy,
      timeUpBound: Int
  ) extends InfosShape {
    override def toString =
      s"Rectangular : width:$width height:$height shift:$shift sign:$sign randomSign:$randomSign withNoise:$withNoise timeStrategy:${timeStrategy.getClass.getSimpleName.init} timeUpBound:$timeUpBound"

    override def getConfig = Seq(
      ("width", width),
      ("height", height),
      ("shift", shift),
      ("sign", sign),
      ("randomSign", randomSign),
      ("withNoise", withNoise),
      ("timeStrategy", timeStrategy),
      ("timeUpBound", timeUpBound)
    )
  }

  case class Triangle(
      width: Int,
      stairShape: (Int, Int),
      shift: Int,
      withNoise: Boolean,
      truncate: Range,
      randomTruncate: Boolean,
      sign: Boolean,
      randomSign: Boolean,
      timeStrategy: Strategy,
      timeUpBound: Int
  ) extends InfosShape {
    override def toString =
      s"Triangle : width:$width stairShape:$stairShape shift:$shift withNoise:$withNoise truncate:${if (truncate == null) "All"
      else s"[${truncate.head}:${truncate.last}]"} randomTruncate:$randomTruncate sign:$sign randomSign:$randomSign " +
        s" timeStrategy:${timeStrategy.getClass.getSimpleName.init} timeUpBound:$timeUpBound"

    override def getConfig = Seq(
      ("width", width),
      ("stairRowShape", stairShape._1),
      ("stairColShape", stairShape._2),
      ("shift", shift),
      ("withNoise", withNoise),
      ("truncate", truncate),
      ("randomTruncate", randomTruncate),
      ("sign", sign),
      ("randomSign", randomSign),
      ("timeStrategy", timeStrategy),
      ("timeUpBound", timeUpBound)
    )
  }

  object RectangularInfos {
    def apply(
        widthRange: Range              = Range.inclusive(128, 256, 32),
        heightRange: Range             = Range.inclusive(128, 256, 32),
        shift: Int                     = 0,
        sign: Boolean                  = true,
        randomSign: Boolean            = false,
        withNoise: Boolean             = false,
        timeStrategy: TimeDiffStrategy = NoneTimeDiff,
        timeUpBound: Int               = 0
    ): Seq[(Seq[ArithInfo], InfosShape)] = {
      widthRange.flatMap { w =>
        heightRange.map { h =>
          (
            genRectangularInfos(
              w,
              h,
              shift,
              sign,
              randomSign,
              withNoise,
              timeStrategy,
              timeUpBound
            ),
            Rectangle(
              w,
              h,
              shift,
              sign,
              randomSign,
              withNoise,
              timeStrategy,
              timeUpBound
            )
          )
        }
      }
    }
  }

  object TriangleInfos {
    def apply(
        widthRange: Range              = Range.inclusive(255, 511, 32),
        stairRowShapeRange: Range      = Range.inclusive(1, 1),
        stairColShapeRange: Range      = Range.inclusive(1, 1),
        shift: Int                     = 0,
        withNoise: Boolean             = false,
        truncate: Range                = null,
        randomTruncate: Boolean        = false,
        sign: Boolean                  = true,
        randomSign: Boolean            = false,
        timeStrategy: TimeDiffStrategy = NoneTimeDiff,
        timeUpBound: Int               = 0
    ): Seq[(Seq[ArithInfo], InfosShape)] = widthRange.flatMap { w =>
      stairRowShapeRange.flatMap { r =>
        stairColShapeRange.map { c =>
          (
            genTriangleInfos(
              w,
              (r, c),
              shift,
              withNoise,
              truncate,
              randomTruncate,
              sign,
              randomSign,
              timeStrategy,
              timeUpBound
            ),
            Triangle(
              w,
              (r, c),
              shift,
              withNoise,
              truncate,
              randomTruncate,
              sign,
              randomSign,
              timeStrategy,
              timeUpBound
            )
          )
        }
      }
    }
  }
}
