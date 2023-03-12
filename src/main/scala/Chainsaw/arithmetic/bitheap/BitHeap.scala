package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class Bit[T](value: T, notComplement: Boolean = true) {

  def hardValue = value.asInstanceOf[Bool]

  def softValue = value.asInstanceOf[BigInt]

  def d(): Bit[Bool] = Bit(hardValue.d(), notComplement)

  def evalBigInt: BigInt =
    if (notComplement) softValue else BigInt(1) - softValue

  def evalBool: Bool = if (notComplement) hardValue else ~hardValue
}

case class BitHeap[T](
    heap: ArrayBuffer[ArrayBuffer[Bit[T]]],
    var weightLow: Int,
    var time: Int
) {

  var constant = BigInt(0)

  type Heap = ArrayBuffer[ArrayBuffer[Bit[T]]]

  /** -------- type hint
    * --------
    */

  def asSoft: BitHeap[BigInt] = this.asInstanceOf[BitHeap[BigInt]]

  def asHard: BitHeap[Bool] = this.asInstanceOf[BitHeap[Bool]]

  def mark = heap
    .find(_.nonEmpty)
    .getOrElse(
      throw new IllegalArgumentException("heap contains no nonempty columns")
    )
    .head

  /** -------- attributes
    * --------
    */

  def complementHeap: Seq[Seq[Boolean]] = heap.map(_.map(_.notComplement))

  def isComplement = heap.forall(_.forall(!_.notComplement))

  def weightHigh = weightLow + heap.length - 1 // weight of the last column

  def width = heap.length

  def heights = heap.map(_.length)

  def heightMax = heights.max

  def bitsCount = heights.sum

  def bitMaxValue = (heights.zipWithIndex.map { case (h, weight) =>
    BigInt(h) << weight
  }.sum << weightLow)

  def maxValue: BigInt =
    (heights.zipWithIndex.map { case (h, weight) =>
      BigInt(h) << weight
    }.sum << weightLow) + constant

  def maxLength = maxValue.bitLength

  def positiveLength = maxValue.bitLength - weightLow

  def minValue: BigInt = BigInt(0)

  def isEmpty = heap.forall(_.isEmpty)

  def nonEmpty = !isEmpty

  def getExactScores(
      compressor: CompressorGenerator,
      columnIndex: Int,
      shouldPipeline: Boolean
  ) = {
    //set compressor pipeline state for get correct scores
    compressor.setPipelineState(shouldPipeline)
    // to get the actual bitIn and its shape
    val bitsCoveredShape =
      compressor.inputFormat // height of columns in input pattern
        .zip(
          heights.drop(columnIndex)
        )                                   // zip with height of columns in this heap
        .map { case (h0, h1) => h0 min h1 } // overlap
    val bits                = bitsCoveredShape.sum
    val cost                = compressor.clbCost
    val reductionEfficiency = (bits - compressor.outputBitsCount) / cost // (bitsIn - bitsOut) / cost
    val reductionRatio      = bits.toDouble / compressor.outputBitsCount
    val bitReduction        = bits - compressor.outputBitsCount
    if (bitsCoveredShape.isEmpty) logger.info(s"$this")
    val heightReduction =
      bitsCoveredShape.max - compressor.outputFormat.max
    compressor.resetPipelineState()
    ScoreIndicator(
      bitReduction,
      heightReduction,
      reductionEfficiency,
      reductionRatio,
      cost
    )
  }

  def reachLastStage = heights.count(_ > 3) < width / 2 && heightMax <= 6

  /** -------- modification methods
    * --------
    */

  def newColumn: ArrayBuffer[Bit[T]] = ArrayBuffer[Bit[T]]()

  // expand the columns according to weight range, this is for the following Bit insertion
  def expand(weightLow: Int, weightHigh: Int): Unit = {
    heap.appendAll(Seq.fill(weightHigh - this.weightHigh)(newColumn))
    heap.prependAll(Seq.fill(this.weightLow - weightLow)(newColumn))
    this.weightLow = weightLow min this.weightLow
    if (weightLow < this.weightLow)
      logger.warn(s"expand to a lower weight $weightLow")
  }

  /** copy all bits from current heap to des, won't clear the current heap
    */
  def copyHeapTo(des: Heap, startCol: Int): Unit = {
    des
      .drop(startCol) // align
      .zip(heap)
      .foreach { case (a, b) => a ++= b } // copy bits
  }

  def moveHeapTo(des: Heap, startCol: Int): Unit = {
    des
      .drop(startCol) // align
      .zip(heap)
      .foreach { case (a, b) =>
        a ++= b
        b.clear()
      } // copy bits
  }

  def addConstant(constant: BigInt) = this.constant += constant

  def absorbPositiveConstant(valueToAdd: BigInt) = {

    val bits = valueToAdd.toString(2).reverse // low to high
    val (constantWeightHigh, constantWeightLow) =
      (bits.length - 1, bits.takeWhile(_ == '0').length)
    val bitsToAdd = bits.dropWhile(_ == '0') // drop tailing zeros
    expand(constantWeightLow, constantWeightHigh)
    heap.drop(constantWeightLow - this.weightLow).zip(bitsToAdd).foreach { case (column, char) =>
      mark.value match {
        case _: Bool =>
          column += Bit(if (char == '1') True else False).asInstanceOf[Bit[T]]
        case _: BigInt =>
          column += Bit(BigInt(char.asDigit)).asInstanceOf[Bit[T]]
      }
    }
  }

  def absorbConstant(): Unit = {
    val exception0 = constant == 0
    val exception1 = constant < 0 && (-constant).mod(pow2(maxLength)) == 0
    if (!exception0 && !exception1) {
      val valueToAdd =
        if (constant >= 0) constant else pow2(maxLength) - (-constant).mod(pow2(maxLength))
      if (constant >= 0) constant = 0
      else constant               = constant - pow2(maxLength) + (-constant).mod(pow2(maxLength)) // clear constant
      absorbPositiveConstant(valueToAdd)
    }
  }

  /** return third heap which is the sum of this and that, won't clear the current heap
    */
  def +(that: BitHeap[T]): BitHeap[T] = {
    require(time == that.time, s"left time = $time, right time = ${that.time}")
    val newLow   = weightLow min that.weightLow
    val newHigh  = weightHigh max that.weightHigh
    val newWidth = newHigh + 1 - newLow

    // initialization
    val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[Bit[T]]())
    this.copyHeapTo(newTable, this.weightLow - newLow)
    that.copyHeapTo(newTable, that.weightLow - newLow)
    val sumHeap = BitHeap(newTable, newLow, time)
    sumHeap.constant = this.constant + that.constant
    sumHeap
  }

  /** move all bits from current heap to des, will clear the current heap
    */
  def contributeHeapTo(des: BitHeap[T]): Unit = {
    require(time == des.time, s"src time = $time, des time = ${des.time}")
    des.expand(des.weightLow min weightLow, des.weightHigh max weightHigh)
    des.weightLow = des.weightLow min weightLow
    val start = weightLow - des.weightLow
    moveHeapTo(des.heap, start)
    des.constant += constant
    des.absorbConstant()
    val emptyColIndex = des.heap.lastIndexWhere(_.isEmpty)
    if (emptyColIndex != -1) des.heap.remove(emptyColIndex)
    require(
      des.heap.forall(_.nonEmpty),
      s"existing empty column!, the column index is ${des.heap.indexWhere(_.isEmpty)}"
    )
    constant = 0
  }

  /** inverse operation of contributeHeapTo
    */
  def absorbHeapFrom(src: BitHeap[T]): Unit = src.contributeHeapTo(this)

  /** all bits with weight >= upper will be dropped
    *
    * @example
    *   weightLow = 2, heights = (1,2,3) -> takeLow(3) -> weightLow = 2, heights
    * = (1)
    */
  def keepLow(upper: Int): Unit = { // TODO: slice
    val remained  = upper - weightLow
    val lowerPart = heap.take(remained)
    heap.clear() // TODO: better modification method?
    lowerPart.copyToBuffer(heap)
  }

  def resize(width: Int): Unit = keepLow(width + weightLow)

  /** -------- implementation(compression) methods
    * --------
    */

  /** take a sub-heap from current heap, according to the given format
    *
    * @return
    *   the sub-heap
    */
  // the result may not fill the whole heap, the compressor is in charge of padding

  private[bitheap] def getSub(format: Seq[Int], columnIdx: Int) = {
    require(heap(columnIdx).nonEmpty, s"src:\n$this\nformat: $format, columnIdx: $columnIdx")
    val newHeap = ArrayBuffer.fill(format.length)(ArrayBuffer[Bit[T]]())
    heap.drop(columnIdx).zip(newHeap).zip(format).foreach { case ((column, newColumn), height) =>
      (0 until height).foreach { _ =>
        if (column.nonEmpty) newColumn += column.remove(0)
      }
    }
    BitHeap(newHeap, weightLow + columnIdx, time)
  }

  /** -------- impl soft methods
    * --------
    */

  def evalBigInt: BigInt = (asSoft.heap
    .map(col => col.map(_.evalBigInt).sum)
    .zipWithIndex
    .map { case (sum, weight) => sum << weight }
    .sum << weightLow) + constant

  def dSoft(): BitHeap[T] = {
    time += 1
    this
  }

  private[bitheap] def implStepSoft(stepSolution: CompressorStepSolution) = {
    val columnIdx = stepSolution.columnIndex
    val format    = stepSolution.getCompressor().inputFormat
    val heapIn    = asSoft.getSub(format, columnIdx)
    val heapOut =
      stepSolution.getCompressor(heapIn.complementHeap).compressSoft(heapIn)
    heapOut
  }

  private[bitheap] def implStageSoft(stageSolution: CompressorStageSolution) = {
    val heapOuts: Seq[BitHeap[BigInt]] = stageSolution.compressorSolutions
      .map(implStepSoft)
    val heapNext: BitHeap[BigInt] = heapOuts.reduce(_ + _)
    asSoft.absorbHeapFrom(heapNext)
    if (stageSolution.pipelined) dSoft()
    if (verbose >= 1)
      logger.info(s"--------soft impl stage output--------\n${this}")
    this
  }

  // TODO: remove this and leave the interface in BitHeapGroup only
  def implAllSoft(solution: CompressorFullSolution): BitHeap[T] = {
    absorbConstant()
    solution.stageSolutions.foreach { StageSolution =>
      asSoft.implStageSoft(StageSolution)
    }
    this
  }

  /** -------- impl hard methods, all these methods are in-place --------
    */

  def dHard(): BitHeap[T] = {
    asHard.heap.foreach { column =>
      column.indices.foreach(i => column(i) = column(i).d())
    }
    dSoft()
    this
  }

  private[bitheap] def implStepHard(stepSolution: CompressorStepSolution) = {
    val columnIdx = stepSolution.columnIndex
    val format    = stepSolution.getCompressor().inputFormat
    val heapIn    = asHard.getSub(format, columnIdx)
    val heapOut =
      stepSolution.getCompressor(heapIn.complementHeap).compressHard(heapIn)
    heapOut
  }

  private[bitheap] def implStageHard(stageSolution: CompressorStageSolution) = {
//    logger.info(s"hard solve heap\n$this")
//    logger.info(s"hard stage solution:$stageSolution")
    val heapOuts: Seq[BitHeap[Bool]] =
      stageSolution.compressorSolutions.map(implStepHard)
    val heapNext: BitHeap[Bool] =
      heapOuts.reduce(_ + _) // combine all heaps generated
    require(isEmpty, s"Incomplete hard impl in stage ${stageSolution.stageIndex}\n$this")
    asHard.absorbHeapFrom(heapNext) // merge the heap generated and the heap remained
    if (stageSolution.pipelined) dHard()
    if (verbose >= 1)
      logger.info(s"--------hard impl stage output--------\n${this}")
    this
  }

  // TODO: remove this and leave the interface in BitHeapGroup only
  def implAllHard(solution: CompressorFullSolution): BitHeap[T] = {
    absorbConstant()
    solution.stageSolutions.foreach { StageSolution =>
      asHard.implStageHard(StageSolution)
    }
    this
  }

  /** -------- other utils --------
    */

  /** allocate a given value to the current heap, old values will be overwritten, for convenience of verification
    */
  def allocate(value: BigInt): Unit = {
    require(value.mod(pow2(weightLow)) == 0)
    var current = value >> weightLow
    asSoft.heap.zipWithIndex.reverse.foreach { case (column, weight) =>
      if (column.isEmpty) {
        if (current >= pow2(weight)) {
          column.prepend(Bit(BigInt(1)))
          current -= pow2(weight)
        }
      } else {
        column.indices.foreach { i =>
          if (current >= pow2(weight)) {
            column(i) = Bit(BigInt(1))
            current -= pow2(weight)
          } else column(i) = Bit(BigInt(0))
        }
      }
    }
  }

  // weightLow is not considered, for full output, weightLow should be used to shift the result
  def toUInts: Seq[UInt] = {
    val ret = ArrayBuffer[UInt]()
    while (heap.exists(_.nonEmpty)) {
      ret += asHard.heap
        .map(col => if (col.nonEmpty) col.remove(0).evalBool else False)
        .asBits()
        .asUInt
    }
    ret
  }

  def toRows[T](zero: T) = {
    val ret = ArrayBuffer[(Seq[T], ArithInfo)]()
    while (heap.exists(_.nonEmpty)) {
      val start        = heap.indexWhere(_.nonEmpty)
      val end          = heap.lastIndexWhere(_.nonEmpty)
      val width        = end - start + 1
      val nonEmptyCols = heap.slice(start, end + 1)
      val row: Seq[T]  = nonEmptyCols.map(col => if (col.nonEmpty) col.remove(0).value.asInstanceOf[T] else zero)
      ret += row -> ArithInfo(width, weightLow + start)
    }
    ret
  }

  def toWeightedBigInts: Seq[WeightedUInt] = toRows(BigInt(0)).map { case (row, arithInfo) =>
    WeightedUInt(
      row.zipWithIndex.map { case (int, i) => int << i }.sum,
      arithInfo
    )
  }

  def toWeightedUInts: Seq[WeightedUInt] = toRows(False).map { case (row, arithInfo) =>
    WeightedUInt(row.asBits().asUInt, arithInfo)
  }

  override def toString = {
    val heapInfoVisualization = s"WeightLow: $weightLow, Time: $time\n"
    val bitsVisualization = heap
      .map(col =>
        col
          .map(bit => if (bit.notComplement) positiveDot else complementDot)
          .padTo(heightMax, " ")
      )
      .reverse
      .transpose
      .map(_.mkString(" "))
      .mkString("\n")
    heapInfoVisualization + bitsVisualization
  }

  def copy = {
    val ret = BitHeap.fromTable(
      heap.map(_.map(bit => bit)).asInstanceOf[Seq[Seq[Bit[BigInt]]]],
      weightLow,
      time
    )
    ret.constant = constant
    ret
  }
}

object BitHeap {

  private def seq2buffer[T](seq: Seq[Seq[T]]): ArrayBuffer[ArrayBuffer[T]] = {
    val buffer = ArrayBuffer.fill(seq.length)(ArrayBuffer[T]())
    seq.zip(buffer).foreach { case (seq, buf) => seq.copyToBuffer(buf) }
    buffer
  }

  def fromTable(
      table: Seq[Seq[Bit[BigInt]]],
      weightLow: Int,
      time: Int
  ): BitHeap[BigInt] =
    BitHeap(seq2buffer(table), weightLow, time)

  /** -------- for GPCs, from columns
    * --------
    */

  def fromHeights(
      heights: Seq[Int],
      weightLow: Int,
      time: Int
  ): BitHeap[BigInt] = {
    val table = heights.map(height => Seq.fill(height)(Bit(BigInt(1, Random))))
    fromTable(table, weightLow, time)
  }

  // for row adders
  def fromRows[T, Operand](
      weightedOperands: Seq[(ArithInfo, Operand)],
      split: (ArithInfo, Operand) => Seq[T]
  ): BitHeap[T] = {
    val time = weightedOperands.head._1.time
    val low  = weightedOperands.map(_._1.low).min
    require(
      weightedOperands.forall(_._1.time == time),
      s"times = ${weightedOperands.map(_._1.time).mkString(" ")}"
    )
    val width    = weightedOperands.map(_._1.high).max + 1 - low
    val heap     = ArrayBuffer.fill(width)(ArrayBuffer[Bit[T]]())
    var constant = BigInt(0)
    weightedOperands.foreach { case (arithInfo, operand) =>
      val positive = arithInfo.isPositive
      val bits     = split(arithInfo, operand)
      heap
        .drop(arithInfo.low - low)
        .zip(bits)
        .zipWithIndex
        .foreach { case ((col, value), i) =>
          col += Bit(value, notComplement = positive)
          if (!positive) constant -= pow2(i + arithInfo.low)
        }
    }
    val ret = BitHeap(heap, low, time)
    ret.addConstant(constant)
    ret
  }

  def fromUInts(weightedUInts: Seq[WeightedUInt]): BitHeap[Bool] = {
    def split(arithInfo: ArithInfo, operand: UInt) =
      operand.asBools // low to high

    fromRows(weightedUInts.map(wU => (wU.arithInfo, wU.value)), split)
  }

  def fromBigInts(weightedBigInts: Seq[WeightedBigInt]): BitHeap[BigInt] = {
    def split(arithInfo: ArithInfo, operand: BigInt) =
      operand.toBitValue(arithInfo.width).asBools // low to high

    fromRows(weightedBigInts.map(wB => (wB.arithInfo, wB.value)), split)
  }

  def fromInfos(arithInfos: Seq[ArithInfo]): BitHeap[BigInt] = fromBigInts(
    arithInfos.map(WeightedBigInt(BigInt(0), _))
  )
}
