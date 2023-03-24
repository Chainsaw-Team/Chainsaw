package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** this class is used to correctly parse bit data based on complement information
  * @param value
  *   the raw data, it can be HardType or SoftType
  * @param notComplement
  *   the complement information, if it's true, it means that the raw data already complement, so parse this bit data
  *   will return raw data, vice versa
  * @tparam T
  *   the raw data type, it can be HardType or SoftType
  */
case class Bit[T](value: T, notComplement: Boolean = true) {

  /** @return
    *   the HardType data
    */
  def hardValue = value.asInstanceOf[Bool]

  /** @return
    *   the SoftType data
    */
  def softValue = value.asInstanceOf[BigInt]

  /** @return
    *   the Bit after delay
    */
  def d(): Bit[Bool] = Bit(hardValue.d(), notComplement)

  /** @return
    *   the real SoftType data after parsing
    */
  def evalBigInt: BigInt =
    if (notComplement) softValue else BigInt(1) - softValue

  /** @return
    *   the real HardType data after parsing
    */
  def evalBool: Bool = if (notComplement) hardValue else ~hardValue
}

/** this class is used to construct the [[BitHeap]] model
  * @param heap
  *   the raw data, it should be a two-dimensional ArrayBuffer of Bit type
  * @param weightLow
  *   the minimum Bit weight of [[BitHeap]]
  * @param time
  *   the arrive time of this [[BitHeap]]
  * @tparam T
  *   the data type of Bit, it should be HardType or SoftType
  */
case class BitHeap[T](
    heap: ArrayBuffer[ArrayBuffer[Bit[T]]],
    var weightLow: Int,
    var time: Int
) {

  var constant = BigInt(0)

  type Heap = ArrayBuffer[ArrayBuffer[Bit[T]]]

  /* -------- type hint--------*/

  /** @return
    *   the SoftType BitHeap
    */
  def asSoft: BitHeap[BigInt] = this.asInstanceOf[BitHeap[BigInt]]

  /** @return
    *   the HardType BitHeap
    */
  def asHard: BitHeap[Bool] = this.asInstanceOf[BitHeap[Bool]]

  /** this method is used to pattern matching
    * @return
    *   the data(Bit) type of this [[BitHeap]]
    */
  def mark = heap
    .find(_.nonEmpty)
    .getOrElse(
      throw new IllegalArgumentException("heap contains no nonempty columns")
    )
    .head

  /* -------- attributes --------*/

  /** this method is used to get the complement state of this [[BitHeap]]
    * @return
    *   the two-dimensional sequence which indicate all Bit's complement state
    */
  def complementHeap: Seq[Seq[Boolean]] = heap.map(_.map(_.notComplement))

  /** get the [[BitHeap]] complement state, if all Bit is already complement, it will return true
    * @return
    *   the Boolean which indicate whether all Bit in this [[BitHeap]] are complemented
    */
  def isComplement = heap.forall(_.forall(!_.notComplement))

  /** the method is used to get the maximum bit weight in this [[BitHeap]]
    * @return
    *   the maximum bit weight of this [[BitHeap]]
    */
  def weightHigh: Int = weightLow + heap.length - 1 // weight of the last column

  /** the method is used to get the bit width of this [[BitHeap]]
    * @return
    *   the bit width of this [[BitHeap]]
    */
  def width = heap.length

  /** the method is used to get all height of column of this [[BitHeap]]
    * @return
    *   a ArrayBuffer which represent all height of column of this [[BitHeap]], The first element represents the first
    *   column's height, and so on
    */
  def heights: ArrayBuffer[Int] = heap.map(_.length)

  /** the method is used to get maximum height of this [[BitHeap]]
    * @return
    *   the maximum height in this [[BitHeap]]
    */
  def heightMax: Int = heights.max

  /** the method is used to get the total amount of Bit in this [[BitHeap]]
    * @return
    *   the total amount of Bit in this [[BitHeap]]
    */
  def bitsCount: Int = heights.sum

  /** the method is used to get the maximum value which all Bit in this [[BitHeap]] can represent(does not include
    * stored constant)
    * @return
    *   the maximum value which all Bit in this [[BitHeap]] can represent
    */
  def bitMaxValue: BigInt = (heights.zipWithIndex.map { case (h, weight) =>
    BigInt(h) << weight
  }.sum << weightLow)

  /** the method is used to get the maximum value which this [[BitHeap]] can represent(include stored constant)
    * @return
    *   the maximum value which this [[BitHeap]] can represent
    */
  def maxValue: BigInt =
    (heights.zipWithIndex.map { case (h, weight) =>
      BigInt(h) << weight
    }.sum << weightLow) + constant

  /** the method is used to get the bitLength of the maxValue of this [[BitHeap]]
    * @return
    *   the bitLength of the maxValue of this [[BitHeap]]
    */
  def maxLength: Int = maxValue.bitLength

  /** the method is used to get the bitLength of the maximum value which the nonEmpty Bit in this BitHeap(exclude the
    * weightLow) can represent
    * @return
    *   the bitLength of nonEmpty Bit in this [[BitHeap]](exclude the weightLow)
    */
  def positiveLength = maxValue.bitLength - weightLow

  /** the method is used to get the minimum value which this [[BitHeap]] can represent
    * @return
    *   the minimum value which this [[BitHeap]] can represent
    */
  def minValue: BigInt = BigInt(0)

  /** the method is used to indicate whether this [[BitHeap]] is empty(does not contain any Bit)
    * @return
    *   the Boolean which indicate whether this [[BitHeap]] is empty
    */
  def isEmpty: Boolean = heap.forall(_.isEmpty)

  /** the method is used to indicate whether this [[BitHeap]] is nonEmpty(contain Bit)
    * @return
    *   the Boolean which indicate whether this [[BitHeap]] is nonEmpty
    */
  def nonEmpty: Boolean = !isEmpty

  /** the method is used to get the scores(evaluation indicators) about compressor applying to this BitHeap
    * @param compressor
    *   the compressor apply to this [[BitHeap]]
    * @param columnIndex
    *   the start column index which this [[BitHeap]] will be cover by compressor
    * @param shouldPipeline
    *   indicate whether this evaluation is in pipeline state
    * @return
    *   the [[ScoreIndicator]] which contain the scores(evaluation indicators) about compressor applying to this BitHeap
    */
  def getExactScores(
      compressor: CompressorGenerator,
      columnIndex: Int,
      shouldPipeline: Boolean
  ): ScoreIndicator = {
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

  /** the method is used to indicate whether this [[BitHeap]] already satisfy the final stage condition
    * @return
    *   the Boolean which indicate whether this [[BitHeap]] already satisfy the final stage condition
    */
  def reachLastStage: Boolean = heights.count(_ > 3) < width / 2 && heightMax <= 6

  /* -------- modification methods --------*/

  /** the method is used to get a empty column of this [[BitHeap]]
    * @return
    *   a empty column of this [[BitHeap]]
    */
  def newColumn: ArrayBuffer[Bit[T]] = ArrayBuffer[Bit[T]]()

  /** this method is used to expand the columns of this [[BitHeap]] according to weight range, this is for the following
    * Bit insertion
    * @param weightLow
    *   the new weightLow after expanding
    * @param weightHigh
    *   the new weightHigh after expanding
    */
  def expand(weightLow: Int, weightHigh: Int): Unit = {
    heap.appendAll(Seq.fill(weightHigh - this.weightHigh)(newColumn))
    heap.prependAll(Seq.fill(this.weightLow - weightLow)(newColumn))
    this.weightLow = weightLow min this.weightLow
    if (weightLow < this.weightLow)
      logger.warn(s"expand to a lower weight $weightLow")
  }

  /** copy all bits from this [[BitHeap]] to des [[BitHeap]], won't clear this heap
    * @param des
    *   the target [[BitHeap]] which will accept this [[BitHeap]]'s bits
    * @param startCol
    *   the start column index, the copy will start in this column
    */
  def copyHeapTo(des: Heap, startCol: Int): Unit = {
    des
      .drop(startCol) // align
      .zip(heap)
      .foreach { case (a, b) => a ++= b } // copy bits
  }

  /** move all bits from this [[BitHeap]] to des [[BitHeap]], will clear this heap
    * @param des
    *   the target [[BitHeap]] which will accept this [[BitHeap]]'s bits
    * @param startCol
    *   the start column index, the movement will start in this column
    */
  def moveHeapTo(des: Heap, startCol: Int): Unit = {
    des
      .drop(startCol) // align
      .zip(heap)
      .foreach { case (a, b) =>
        a ++= b
        b.clear()
      } // copy bits
  }

  /** this method is used to add a constant to this [[BitHeap]]
    * @param constant
    *   the constant which will be added
    */
  def addConstant(constant: BigInt): Unit = this.constant += constant

  /** this method is used to absorb a positive constant to this [[BitHeap]], the constant will convert to bits and add
    * to this [[BitHeap]]
    * @param valueToAdd
    *   the positive constant which will be absorbed
    */
  def absorbPositiveConstant(valueToAdd: BigInt): Unit = {

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

  /** this method is used to absorb the constant in this [[BitHeap]], the constant will convert to bits and add to this
    * [[BitHeap]]
    */
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
    * @param that
    *   the heap which will sum with this [[BitHeap]]
    * @return
    *   the sum of two [[BitHeap]]
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

  /** move all bits from this [[BitHeap]] to des [[BitHeap]], will clear all bits of this [[BitHeap]]
    * @param des
    *   the [[BitHeap]] which will accept bits
    */
  def contributeHeapTo(des: BitHeap[T]): Unit = {
    require(time == des.time, s"src time = $time, des time = ${des.time}")
    des.expand(des.weightLow min weightLow, des.weightHigh max weightHigh)
    des.weightLow = des.weightLow min weightLow
    val start = weightLow - des.weightLow
    moveHeapTo(des.heap, start)
    des.constant += constant
    des.absorbConstant()
    val lastEmptyColIndex    = des.heap.lastIndexWhere(_.isEmpty)
    val lastNonEmptyColIndex = des.heap.lastIndexWhere(_.nonEmpty)
    if (lastEmptyColIndex > lastNonEmptyColIndex) {
      logger.info(
        s"existing empty column in leftmost column!, the column indices is [$lastNonEmptyColIndex : $lastEmptyColIndex]"
      )
      (lastNonEmptyColIndex + 1 to lastNonEmptyColIndex).foreach(des.heap.remove)
    }
    constant = 0
  }

  /** inverse operation of contributeHeapTo
    * @param src
    *   the [[BitHeap]] which will contribute bits
    */
  def absorbHeapFrom(src: BitHeap[T]): Unit = src.contributeHeapTo(this)

  /** take low bits of this [[BitHeap]], all bits with weight >= upper will be dropped
    * @param upper
    *   the maximum index which will be remained
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

  /** this method is used to resize the width of this [[BitHeap]]
    * @param width
    *   the new width after resizing
    */
  def resize(width: Int): Unit = keepLow(width + weightLow)

  /* -------- implementation(compression) methods-------- */

  /** this method is used to take a sub-[[BitHeap]] from this heap, according to the given format
    * @param format
    *   the sub-[[BitHeap]] format
    * @param columnIdx
    *   the start column index which this method take bits begin at
    * @return
    *   the sub-[[BitHeap]]
    */
  private[bitheap] def getSub(format: Seq[Int], columnIdx: Int): BitHeap[T] = {
    require(heap(columnIdx).nonEmpty, s"src:\n$this\nformat: $format, columnIdx: $columnIdx")
    val newHeap = ArrayBuffer.fill(format.length)(ArrayBuffer[Bit[T]]())
    heap.drop(columnIdx).zip(newHeap).zip(format).foreach { case ((column, newColumn), height) =>
      (0 until height).foreach { _ =>
        if (column.nonEmpty) newColumn += column.remove(0)
      }
    }
    BitHeap(newHeap, weightLow + columnIdx, time)
  }

  /* -------- impl soft methods -------- */

  /** this method is used to get the real softType value which this [[BitHeap]] represent
    * @return
    *   the real SoftType value of this [[BitHeap]]
    */
  def evalBigInt: BigInt = (asSoft.heap
    .map(col => col.map(_.evalBigInt).sum)
    .zipWithIndex
    .map { case (sum, weight) => sum << weight }
    .sum << weightLow) + constant

  /** this method is used to add delay to this SoftType BitHeap
    * @return
    *   the delayed SoftType BitHeap
    */
  def dSoft(): BitHeap[T] = {
    time += 1
    this
  }

  /** this method is used to compress this SoftType [[BitHeap]] once by a compressor which parsing from
    * [[CompressorStepSolution]]
    * @param stepSolution
    *   a [[CompressorStepSolution]] to guide this compress
    * @return
    *   the output SoftType [[BitHeap]] of the compressor, its the sub-[[BitHeap]] of stage compress result
    */
  private[bitheap] def implStepSoft(stepSolution: CompressorStepSolution): BitHeap[BigInt] = {
    val columnIdx = stepSolution.columnIndex
    val format    = stepSolution.getCompressor().inputFormat
    val heapIn    = asSoft.getSub(format, columnIdx)
    val heapOut =
      stepSolution.getCompressor(heapIn.complementHeap).compressSoft(heapIn)
    heapOut
  }

  /** this method is used to compress this SoftType [[BitHeap]] one stage by a certain number of compressor which
    * parsing from [[CompressorStageSolution]]
    * @param stageSolution
    *   a [[CompressorStageSolution]] to guide this compress
    * @return
    *   the SoftType [[BitHeap]] output of this stage compress
    */
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
  /** this method is used to compress this SoftType [[BitHeap]] by all compressors which parsing from
    * [[CompressorFullSolution]]
    * @param solution
    *   a [[CompressorFullSolution]] to guide this compress
    * @return
    *   the SoftType [[BitHeap]] output of final stage compress
    */
  def implAllSoft(solution: CompressorFullSolution): BitHeap[T] = {
    absorbConstant()
    solution.stageSolutions.foreach { StageSolution =>
      asSoft.implStageSoft(StageSolution)
    }
    this
  }

  /** -------- impl hard methods, all these methods are in-place -------- */
  /** this method is used to add delay to this HardType [[BitHeap]]
    * @return
    *   the delayed HardType [[BitHeap]]
    */
  def dHard(): BitHeap[T] = {
    asHard.heap.foreach { column =>
      column.indices.foreach(i => column(i) = column(i).d())
    }
    dSoft()
    this
  }

  /** this method is used to compress this HardType [[BitHeap]] once by a compressor which parsing from
    * [[CompressorStepSolution]]
    * @param stepSolution
    *   a [[CompressorStepSolution]] to guide this compress
    * @return
    *   the output HardType [[BitHeap]] of the compressor, its the sub-[[BitHeap]] of stage compress result
    */
  private[bitheap] def implStepHard(stepSolution: CompressorStepSolution) = {
    val columnIdx = stepSolution.columnIndex
    val format    = stepSolution.getCompressor().inputFormat
    val heapIn    = asHard.getSub(format, columnIdx)
    val heapOut =
      stepSolution.getCompressor(heapIn.complementHeap).compressHard(heapIn)
    heapOut
  }

  /** this method is used to compress this HardType [[BitHeap]] one stage by a certain number of compressor which
    * parsing from [[CompressorStageSolution]]
    * @param stageSolution
    *   a [[CompressorStageSolution]] to guide this compress
    * @return
    *   the HardType [[BitHeap]] output of this stage compress
    */
  private[bitheap] def implStageHard(stageSolution: CompressorStageSolution) = {
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
  /** this method is used to compress this HardType [[BitHeap]] by all compressors which parsing from
    * [[CompressorFullSolution]]
    * @param solution
    *   a [[CompressorFullSolution]] to guide this compress
    * @return
    *   the HardType [[BitHeap]] output of final stage compress
    */
  def implAllHard(solution: CompressorFullSolution): BitHeap[T] = {
    absorbConstant()
    solution.stageSolutions.foreach { StageSolution =>
      asHard.implStageHard(StageSolution)
    }
    this
  }

  /** -------- other utils -------- */

  /** this method is used to allocate a given value to this heap, old values will be overwritten, for convenience of
    * verification
    * @param value
    *   the given value will be allocate to this [[BitHeap]]
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

  //

  /** this method is used to transform this HardType [[BitHeap]] to a sequence of UInt type, weightLow is not
    * considered, for full output, weightLow should be used to shift the result
    * @return
    *   the sequence of UInt type which represent the UInt formed by all bits in every row
    */
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

  /** this method is used to transform this [[BitHeap]] to a sequence of Tuple[Seq[Bit], [[ArithInfo]] type, it contain
    * the Seq[Bit] formed by all bits in every row with its [[ArithInfo]] information
    * @return
    *   the sequence of Tuple[Seq[Bit], [[ArithInfo]] type which represent the Seq[Bit] formed by all bits in every row
    *   with its [[ArithInfo]] information
    */
  def toRows[T](zero: T): ArrayBuffer[(Seq[T], ArithInfo)] = {
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

  /** this method is used to transform this SoftType [[BitHeap]] to a sequence of [[WeightedBigInt]] type
    * @return
    *   the sequence of [[WeightedBigInt]] type
    */
  def toWeightedBigInts: Seq[WeightedBigInt] = toRows(BigInt(0)).map { case (row, arithInfo) =>
    WeightedBigInt(
      row.zipWithIndex.map { case (int, i) => int << i }.sum,
      arithInfo
    )
  }

  /** this method is used to transform this HardType [[BitHeap]] to a sequence of [[WeightedUInt]] type
    * @return
    *   the sequence of [[WeightedUInt]] type
    */
  def toWeightedUInts: Seq[WeightedUInt] = toRows(False).map { case (row, arithInfo) =>
    WeightedUInt(row.asBits().asUInt, arithInfo)
  }

  /** override the toString method, it is used to visualize this [[BitHeap]]
    * @return
    *   the visualized String of this [[BitHeap]]
    */
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

  /** this method is used to deep copy this [[BitHeap]]
    * @return
    *   the deep copy of this [[BitHeap]]
    */
  def copy: BitHeap[BigInt] = {
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

  /** this method is used to transform a two-dimensional seq to a two-dimensional arrayBuffer
    * @param seq
    *   the seq will be transformed
    * @tparam T
    *   the data type
    * @return
    *   the two-dimensional arrayBuffer after transforming
    */
  private def seq2buffer[T](seq: Seq[Seq[T]]): ArrayBuffer[ArrayBuffer[T]] = {
    val buffer = ArrayBuffer.fill(seq.length)(ArrayBuffer[T]())
    seq.zip(buffer).foreach { case (seq, buf) => seq.copyToBuffer(buf) }
    buffer
  }

  /** this method is used to construct a SoftType [[BitHeap]] according to given table information
    * @param table
    *   the table which contain all bits of the result SoftType [[BitHeap]]
    * @param weightLow
    *   the weightLow of result SoftType [[BitHeap]]
    * @param time
    *   the arrive time of result SoftType [[BitHeap]]
    * @return
    *   the SoftType [[BitHeap]] constructed from given information
    */
  def fromTable(
      table: Seq[Seq[Bit[BigInt]]],
      weightLow: Int,
      time: Int
  ): BitHeap[BigInt] =
    BitHeap(seq2buffer(table), weightLow, time)

  /* -------- for GPCs, from columns -------- */

  /** this method is used to construct a SoftType [[BitHeap]] according to given heights information
    * @param heights
    *   the heights which contain all column height of result SoftType [[BitHeap]]
    * @param weightLow
    *   the weightLow of result SoftType [[BitHeap]]
    * @param time
    *   the arrive time of result SoftType [[BitHeap]]
    * @return
    *   the SoftType [[BitHeap]] constructed from given information
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
  /** this method is used to construct a [[BitHeap]] according to given row information
    * @param weightedOperands
    *   the operands(rows) which contain weight information for construct [[BitHeap]]
    * @param split
    *   the split method will apply to this construct
    * @return
    *   the [[BitHeap]] constructed from given information
    */
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

  /** this method is used to construct a HardType [[BitHeap]] according to given UInt information
    * @param weightedUInts
    *   the UInts(rows) which contain weight information for construct HardType [[BitHeap]]
    * @return
    *   the HardType [[BitHeap]] constructed from given information
    */
  def fromUInts(weightedUInts: Seq[WeightedUInt]): BitHeap[Bool] = {
    def split(arithInfo: ArithInfo, operand: UInt) =
      operand.asBools // low to high

    fromRows(weightedUInts.map(wU => (wU.arithInfo, wU.value)), split)
  }

  /** this method is used to construct a SoftType [[BitHeap]] according to given BigInts information
    * @param weightedBigInts
    *   the BigInts(rows) which contain weight information for construct SoftType [[BitHeap]]
    * @return
    *   the SoftType [[BitHeap]] constructed from given information
    */
  def fromBigInts(weightedBigInts: Seq[WeightedBigInt]): BitHeap[BigInt] = {
    def split(arithInfo: ArithInfo, operand: BigInt) =
      operand.toBitValue(arithInfo.width).asBools // low to high

    fromRows(weightedBigInts.map(wB => (wB.arithInfo, wB.value)), split)
  }

  /** this method is used to construct a SoftType [[BitHeap]] according to given [[ArithInfo]] information
    * @param arithInfos
    *   the arithInfos(rows) which contain weight information for construct SoftType [[BitHeap]]
    * @return
    *   the SoftType [[BitHeap]] constructed from given information
    */
  def fromInfos(arithInfos: Seq[ArithInfo]): BitHeap[BigInt] = fromBigInts(
    arithInfos.map(WeightedBigInt(BigInt(0), _))
  )
}
