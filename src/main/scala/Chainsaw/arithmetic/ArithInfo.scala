package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._

import scala.collection.immutable
import scala.util.Random
import bitheap.BitHeap

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

  /** this method is used indicate this operand whether is negative
    * @return
    *   the Boolean which indicate this operand whether is negative
    */
  def isNegative = !isPositive

  require(weight >= 0, "weight must be non-negative")

  /** this method is used to get the lowest index of this operand
    * @return
    *   the lowest index of this operand
    */
  def low: Int = weight

  // TODO: high = low + width may be more reasonable
  /** this method is used to get the highest index of this operand
    * @return
    *   the highest index of this operand
    */
  def high: Int = low + width - 1

  /** this method is used to get the index range of this operand
    * @return
    *   the index range of this operand
    */
  def range: Range.Inclusive = high downto low

  /** this method is used to get the maximum value which this operand can represent
    * @return
    *   the maximum value which this operand can represent
    */
  def maxValue: BigInt = if (isPositive) (pow2(width) - 1) << weight else BigInt(0)

  /** this method is used to get a new [[ArithInfo]] which is the shiftLeft version of this [[ArithInfo]]
    * @param shiftLeft
    *   the length of shiftLeft
    * @return
    *   a new [[ArithInfo]] which is the shiftLeft version of this [[ArithInfo]]
    */
  def <<(shiftLeft: Int) = ArithInfo(width, weight + shiftLeft, isPositive)

  /** this method is used to get a new [[ArithInfo]] which is the shiftRight version of this [[ArithInfo]]
    * @param shiftRight
    *   the length of shiftRight
    * @return
    *   a new [[ArithInfo]] which is the shiftRight version of this [[ArithInfo]]
    */
  def >>(shiftRight: Int): ArithInfo = <<(-shiftRight)

  /** this method is used to get a new [[ArithInfo]] which is the inverted version of this [[ArithInfo]]
    * @return
    *   a new [[ArithInfo]] which is the inverted version of this [[ArithInfo]]
    */
  def unary_- = ArithInfo(width, weight, !isPositive)

  /** this method is used to parse a value by this [[ArithInfo]]'s information
    * @param value
    *   the value which will be parsed
    * @return
    *   the new value after parsing
    */
  def eval(value: BigInt): BigInt = {
    assert(
      value.bitLength <= width,
      s"yours: ${value.bitLength}, upper: $width"
    )
    (value << weight) * (if (isPositive) 1 else -1)
  }

  /** this method is used to add carry to this operand
    * @param carry
    *   the length of carry which will be added
    * @return
    *   a new [[ArithInfo]] after adding the carry to this [[ArithInfo]]
    */
  def withCarry(carry: Int): ArithInfo = ArithInfo(width + carry, weight, isPositive, time)

  /** this method is used to set the operand's arrival time
    * @param newTime
    *   the new operand's arrival time
    * @return
    *   a new [[ArithInfo]] with new arrival time
    */
  def withTime(newTime: Int): ArithInfo = ArithInfo(width, weight, isPositive, newTime)

  /** this method is used to convert this [[ArithInfo]] to a [[ArithInfo]] with positive sign
    * @return
    *   the positive sign version of this [[ArithInfo]]
    */
  def toPositive: ArithInfo = ArithInfo(width, weight, isPositive = true, time)

  /* -------- FIXME: following methods are for Bm only -------- */

  /** this method can be used to split this [[ArithInfo]] into N segment
    * @param n
    *   the number of segment
    * @return
    *   a sequence of [[ArithInfo]] which contain all ArithInfo segment of this [[ArithInfo]]
    */
  def splitN(n: Int): Seq[ArithInfo] = {
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

  /** this method can be used to split this [[ArithInfo]] into two segment, first one is the MSB of the operand which
    * this [[ArithInfo]] represent, second one is the remaining [[ArithInfo]]
    * @return
    *   a Tuple which first element is the MSB of the operand which this [[ArithInfo]] represent, second element is the
    *   remaining [[ArithInfo]]
    */
  def splitMsb: (ArithInfo, ArithInfo) = {
    require(isPositive)
    (
      ArithInfo(width = 1, weight, isPositive         = true, time),
      ArithInfo(width = width - 1, weight, isPositive = true, time)
    )
  }

  /** the addition method of [[ArithInfo]]
    * @param that
    *   the [[ArithInfo]] will be added to this [[ArithInfo]]
    * @return
    *   a new [[ArithInfo]] obtained by adding two [[ArithInfo]] together
    */
  def +(that: ArithInfo): ArithInfo = {
    require(this.width == that.width)
    ArithInfo(
      width + 1,
      weight,
      isPositive,
      time + width.divideAndCeil(cpaWidthMax)
    )
  }

  /** the multiplication method of [[ArithInfo]]
    * @param that
    *   the [[ArithInfo]] will be multiplied with this [[ArithInfo]]
    * @return
    *   a new [[ArithInfo]] obtained by multiplying two [[ArithInfo]] together
    */
  def *(that: ArithInfo): ArithInfo = {
    ArithInfo(
      this.width + that.width,
      weight + that.weight,
      isPositive,
      time + 2
    )
  }

  /** the and method of [[ArithInfo]]
    * @param that
    *   the [[ArithInfo]] will be and with this [[ArithInfo]]
    * @return
    *   a new [[ArithInfo]] obtained by and two [[ArithInfo]] together
    */
  def &(that: ArithInfo): ArithInfo = {
    require(that.width == 1)
    ArithInfo(width, weight + that.weight, isPositive, time)
  }

  /** this method is used to merge this [[ArithInfo]] with a sequence of [[ArithInfo]]
    * @param tail
    *   the sequence of [[ArithInfo]] will be merge into this [[ArithInfo]]
    * @param widthOut
    *   the new [[ArithInfo]]'s width
    * @return
    *   a new [[ArithInfo]] obtained by merging this [[ArithInfo]] and other [[ArithInfo]]
    */
  def mergeWith(tail: Seq[ArithInfo], widthOut: Int): ArithInfo = {
    val all  = this +: tail
    val base = all.map(_.weight).min
    ArithInfo(widthOut, base, isPositive = true, all.map(_.time).max)
  }

  /** override toString method to get the information we need
    * @return
    *   String contain the information about this operand
    */
  override def toString =
    s"${if (isPositive) "positive" else "negative"} $width-bit<<$weight at $time"
}

/** this class is used to store the BigInt and its arithmetic information, , used for simulating UInt arithmetic *
  * @param value
  *   the raw bigInt will be parsed
  * @param arithInfo
  *   the data format which the raw data will be parsed by
  */
case class WeightedBigInt(value: BigInt, arithInfo: ArithInfo) {
  require(
    value.bitLength <= arithInfo.width,
    s"the value width ${value.bitLength} is too large for the target width ${arithInfo.width}"
  )

  def eval: BigInt = arithInfo.eval(value)

  /** this method is used to get a new [[WeightedBigInt]] which is the shiftLeft version of this [[WeightedBigInt]]
    * @param shiftLeft
    *   the length of shiftLeft
    * @return
    *   a new [[WeightedBigInt]] which is the shiftLeft version of this [[WeightedBigInt]]
    */
  def <<(shiftLeft: Int) = WeightedBigInt(value, arithInfo << shiftLeft)

  /** this method is used to get a new [[WeightedBigInt]] which is the inverted version of this [[WeightedBigInt]]
    * @return
    *   a new [[WeightedBigInt]] which is the inverted version of this [[WeightedBigInt]]
    */
  def unary_- = WeightedBigInt(value, -arithInfo)

  /** this method is used to refresh the weight of this [[WeightedBigInt]]
    * @param weight
    *   the new weight of this [[WeightedBigInt]]
    * @return
    *   a new [[WeightedBigInt]] with new weight
    */
  def withWeight(weight: Int) = WeightedBigInt(
    value,
    ArithInfo(arithInfo.width, weight, arithInfo.isPositive, arithInfo.time)
  )

}

/** this class is used to store the UInt and its arithmetic information, used for generating [[BitHeap]]
  * @param value
  *   the raw UInt will be parsed
  * @param arithInfo
  *   the data format which the raw data will be parsed by
  */
case class WeightedUInt(value: UInt, arithInfo: ArithInfo) {
  require(
    value.getBitsWidth <= arithInfo.width,
    s"the value width ${value.getBitsWidth} is too large for the target width ${arithInfo.width}"
  )

  /** this method is used to get a new [[WeightedUInt]] which is the shiftLeft version of this [[WeightedUInt]]
    * @param shiftLeft
    *   the length of shiftLeft
    * @return
    *   a new [[WeightedUInt]] which is the shiftLeft version of this [[WeightedUInt]]
    */
  def <<(shiftLeft: Int) = WeightedUInt(value, arithInfo << shiftLeft)

  /** this method is used to get a new [[WeightedUInt]] which is the inverted version of this [[WeightedUInt]]
    * @return
    *   a new [[WeightedUInt]] which is the inverted version of this [[WeightedUInt]]
    */
  def unary_- = WeightedUInt(value, -arithInfo)

  /** this method is used to refresh the weight of this [[WeightedUInt]]
    * @param weight
    *   the new weight of this [[WeightedUInt]]
    * @return
    *   a new [[WeightedUInt]] with new weight
    */
  def withWeight(weight: Int) = WeightedUInt(
    value,
    ArithInfo(arithInfo.width, weight, arithInfo.isPositive, arithInfo.time)
  )
}

/** the object is used to generate some arithmetic information about operand, it's useful in constructing testCase
  */
object ArithInfoGenerator {

  /* -------- random bit heap generators -------- */

  /** this method is used to generate a random number from -bound to bound
    * @param bound
    *   the random number's upper bound and lower bound
    * @return
    *   the random number in range -bound ~ bound
    */
  def genNoise(bound: Int): Int =
    Random.nextInt(2 * bound + 1) - bound // -bound ~ bound

  /** this method is used to generate a sequence of [[ArithInfo]] which describe a rectangular [[BitHeap]] by the
    * configuration information, typically generated by a adder.
    * @param width
    *   the maximum width of this rectangular [[BitHeap]]
    * @param height
    *   the maximum height of this rectangular [[BitHeap]]
    * @param shift
    *   the shift length of every [[ArithInfo]]
    * @param sign
    *   the sign of every generated [[ArithInfo]]
    * @param randomSign
    *   the switch of random sign pattern, if true, the generated [[ArithInfo]] will have random sign
    * @param withNoise
    *   the switch of noise pattern, if true, the width or height of rectangular [[BitHeap]] described by generated
    *   sequence of [[ArithInfo]] will be added noise
    * @param timeStrategy
    *   the time generate strategy, it can be NoneTimeDiff, IncreaseTimeDiff, DecreaseTimeDiff and RandomTimeDiff
    * @param timeUpBound
    *   the upper bound of the arrival time of different [[ArithInfo]]
    * @return
    *   a sequence of [[ArithInfo]] which describe a rectangular [[BitHeap]] according to configuration information
    */
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

  /** this method is used to generate a sequence of [[ArithInfo]] which describe a inverted triangle [[BitHeap]] by the
    * configuration information, typically generated by a multiplier.
    * @param width
    *   the width of this inverted triangle [[BitHeap]]
    * @param stairShape
    *   the tuple define the horizontal and vertical ladder length
    * @param shift
    *   the integral shift of this inverted triangle [[BitHeap]]
    * @param withNoise
    *   the switch of noise pattern, if true, the width or height of triangle [[BitHeap]] described by generated
    *   sequence of [[ArithInfo]] will be added noise
    * @param truncate
    *   the truncate range, if it's not null, this method will return the truncated version of this inverted triangle
    *   [[BitHeap]]
    * @param randomTruncate
    *   the switch of random truncate pattern, if true, this method will generate a random truncated version of this
    *   inverted triangle
    * @param sign
    *   the sign of every generated [[ArithInfo]]
    * @param randomSign
    *   the switch of random sign pattern, if true, the generated [[ArithInfo]] will have random sign
    * @param timeStrategy
    *   the time generate strategy, it can be NoneTimeDiff, IncreaseTimeDiff, DecreaseTimeDiff and RandomTimeDiff
    * @param timeUpBound
    *   the upper bound of the arrival time of different [[ArithInfo]]
    * @return
    *   a sequence of [[ArithInfo]] which describe a triangle [[BitHeap]] according to configuration information
    */
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

  /* -------- graph generation utils -------- */

  /** the abstract class which represent the information of [[BitHeap]] shape, used for graph generation
    */
  abstract class InfosShape {

    /** this method is used to get a sequence of tuple which represent the configuration information of this shape
      * @return
      *   a sequence of tuple which represent the configuration information of this shape
      */
    def getConfig: Seq[(String, Any)]
  }

  /** this class used to represent the information of a rectangle shape BitHeap
    * @param width
    *   the maximum width of this rectangular [[BitHeap]]
    * @param height
    *   the maximum height of this rectangular [[BitHeap]]
    * @param shift
    *   the shift length of every [[ArithInfo]]
    * @param sign
    *   the sign of every generated [[ArithInfo]]
    * @param randomSign
    *   the switch of random sign pattern, if true, the generated [[ArithInfo]] will have random sign
    * @param withNoise
    *   the switch of noise pattern, if true, the width or height of rectangular [[BitHeap]] described by generated
    *   sequence of [[ArithInfo]] will be added noise
    * @param timeStrategy
    *   the time generate strategy, it can be NoneTimeDiff, IncreaseTimeDiff, DecreaseTimeDiff and RandomTimeDiff
    * @param timeUpBound
    *   the upper bound of the arrival time of different [[ArithInfo]]
    */
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

  /** this class used to represent the information of a triangle shape BitHeap
    * @param width
    *   the width of this inverted triangle [[BitHeap]]
    * @param stairShape
    *   the tuple define the horizontal and vertical ladder length
    * @param shift
    *   the integral shift of this inverted triangle [[BitHeap]]
    * @param withNoise
    *   the switch of noise pattern, if true, the width or height of triangle [[BitHeap]] described by generated
    *   sequence of [[ArithInfo]] will be added noise
    * @param truncate
    *   the truncate range, if it's not null, this method will return the truncated version of this inverted triangle
    *   [[BitHeap]]
    * @param randomTruncate
    *   the switch of random truncate pattern, if true, this method will generate a random truncated version of this
    *   inverted triangle
    * @param sign
    *   the sign of every generated [[ArithInfo]]
    * @param randomSign
    *   the switch of random sign pattern, if true, the generated [[ArithInfo]] will have random sign
    * @param timeStrategy
    *   the time generate strategy, it can be NoneTimeDiff, IncreaseTimeDiff, DecreaseTimeDiff and RandomTimeDiff
    * @param timeUpBound
    *   the upper bound of the arrival time of different [[ArithInfo]]
    */
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

    /** the method of batch invoke [[genRectangularInfos]] to get multiple sequence of [[ArithInfo]] which represent
      * rectangular [[BitHeap]]
      * @param widthRange
      *   the width range of generated rectangular BitHeap
      * @param heightRange
      *   the height range of generated rectangular BitHeap
      * @param shift
      *   see the shift of [[genTriangleInfos]]
      * @param sign
      *   see the sign of [[genTriangleInfos]]
      * @param randomSign
      *   see the randomSign of [[genTriangleInfos]]
      * @param withNoise
      *   see the withNoise of [[genTriangleInfos]]
      * @param timeStrategy
      *   see the timeStrategy of [[genTriangleInfos]]
      * @param timeUpBound
      *   see the timeUpBound of [[genTriangleInfos]]
      * @return
      *   a two dimensional sequence of [[ArithInfo]] which represent multiple rectangular [[BitHeap]]
      */
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

    /** the method of batch invoke [[genRectangularInfos]] to get multiple sequence of [[ArithInfo]] which represent
      * triangle [[BitHeap]]
      * @param widthRange
      *   the width range of generated triangle BitHeap
      * @param stairRowShapeRange
      *   the stairRowShape(stairShape._1 in [[genTriangleInfos]]) range of generated triangle BitHeap
      * @param stairColShapeRange
      *   the stairColShapeRange(stairShape._2 in [[genTriangleInfos]]) range of generated triangle BitHeap
      * @param shift
      *   see the shift in [[genTriangleInfos]]
      * @param withNoise
      *   see the withNoise in [[genTriangleInfos]]
      * @param truncate
      *   see the truncate in [[genTriangleInfos]]
      * @param randomTruncate
      *   see the randomTruncate in [[genTriangleInfos]]
      * @param sign
      *   see the sign in [[genTriangleInfos]]
      * @param randomSign
      *   see the randomSign in [[genTriangleInfos]]
      * @param timeStrategy
      *   see the timeStrategy in [[genTriangleInfos]]
      * @param timeUpBound
      *   see the timeUpBound in [[genTriangleInfos]]
      * @return
      */
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
