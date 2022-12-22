package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class BitHeap[T](heap: ArrayBuffer[ArrayBuffer[T]], var weightLow: Int, var time: Int) {

  type Heap = ArrayBuffer[ArrayBuffer[T]]

  def asSoft = this.asInstanceOf[BitHeap[BigInt]]

  def asHard = this.asInstanceOf[BitHeap[Bool]]

  /** --------
   * attributes
   * -------- */
  def weightHigh = weightLow + heap.length - 1

  def width = heap.length

  def heights = heap.map(_.length)

  def heightMax = heights.max

  def bitsCount = heights.sum

  def maxValue: BigInt = heights.zipWithIndex.map { case (h, weight) => BigInt(h) << weight }.sum << weightLow

  def minValue: BigInt = BigInt(0)

  def isEmpty = heap.forall(_.isEmpty)

  def nonEmpty = !isEmpty

  def precision = Pow2(weightLow)

  def mark = heap.head.head

  /** --------
   * modification methods
   * -------- */

  private def representable(value: BigInt) = value.mod(precision) == 0

  def addPositiveConstant(constant: BigInt): Unit = {
    require(constant >= 0 && representable(constant), s"constant $constant is not representable")
    val bits = (constant >> weightLow).toString(2).reverse
    bits.zipWithIndex.foreach { case (char, i) =>
      val col: ArrayBuffer[T] = if (heap.isDefinedAt(i)) heap(i) else {
        heap += ArrayBuffer[T]()
        heap.last
      }
      mark match {
        case _: BigInt => col += BigInt(char.asDigit).asInstanceOf[T]
        case _: Bool => col += Bool(char.asDigit == 1).asInstanceOf[T]
      }
    }
  }

  def addNegativeConstant(constant: BigInt) = {
    require(constant < 0 && representable(-constant))
    require(maxValue >= -constant, "meaningless to add a negative constant larger than the max value")
    val validLength = (maxValue + constant).bitLength
    val complement = Pow2(maxValue.bitLength) + constant
    addPositiveConstant(complement)
    validLength
  }

  /** copy all bits from current heap to des, won't clear the current heap
   */
  def copyHeapTo(des: Heap, start: Int): Unit = {
    des.drop(start) // align
      .zip(heap).foreach { case (a, b) => a ++= b } // copy bits
  }

  /** return third heap which is the sum of this and that, won't clear the current heap
   */
  def +(that: BitHeap[T]): BitHeap[T] = {
    require(time == that.time)

    val newLow = weightLow min that.weightLow
    val newHigh = weightHigh max that.weightHigh
    val newWidth = newHigh + 1 - newLow

    // initialization
    val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
    this.copyHeapTo(newTable, this.weightLow - newLow)
    that.copyHeapTo(newTable, that.weightLow - newLow)
    BitHeap(newTable, newLow, time)
  }


  /** move all bits from current heap to des, will clear the current heap
   */
  def contributeHeapTo(des: BitHeap[T]): Unit = {
    val lowDiff = des.weightLow - weightLow
    if (lowDiff > 0) {
      des.heap.prepend(Seq.fill(lowDiff)(ArrayBuffer[T]()): _*)
      des.weightLow -= lowDiff
    }
    val highDiff = des.weightHigh - weightHigh
    if (highDiff < 0) des.heap.append(Seq.fill(-highDiff)(ArrayBuffer[T]()): _*)

    val start = -lowDiff max 0
    des.heap.drop(start).zip(heap) // align and zip
      .foreach { case (a, b) =>
        a ++= b
        b.clear()
      } // copy bits
    heap.clear() // clear bits
  }

  /** inverse operation of contributeHeapTo
   */
  def absorbHeapFrom(src: BitHeap[T]): Unit = src.contributeHeapTo(this)

  /** take a sub-heap from current heap, according to the given format
   *
   * @param format    bitheap format described by a sequence of  heights
   * @param columnIdx column index where the sub-heap starts
   * @return the sub-heap
   */
  // the result may not fill the whole heap, the compressor is in charge of padding
  def getSub(format: Seq[Int], columnIdx: Int) = {
    require(heap(columnIdx).nonEmpty)
    val newHeap = ArrayBuffer.fill(format.length)(ArrayBuffer[T]())
    heap.drop(columnIdx).zip(newHeap).zip(format).foreach { case ((column, newColumn), height) =>
      (0 until height).foreach { _ => if (column.nonEmpty) newColumn += column.remove(0) }
    }
    BitHeap(newHeap, weightLow + columnIdx, time)
  }

  /** allocate a given value to the current heap, old values will be overwritten
   */
  def allocate(value: BigInt): Unit = {
    require(value.mod(Pow2(weightLow)) == 0)
    var current = value >> weightLow
    asSoft.heap.zipWithIndex.reverse.foreach { case (column, weight) =>
      column.indices.foreach { i =>
        if (current >= Pow2(weight)) {
          column(i) = BigInt(1)
          current -= Pow2(weight)
        } else column(i) = BigInt(0)
      }
    }
  }

  /** all bits with weight >= upper will be dropped
   *
   * @example weightLow = 2, heights = (1,2,3) -> takeLow(3) -> weightLow = 2, heights = (1)
   */
  def keepLow(upper: Int): Unit = { // TODO: slice
    val remained = upper - weightLow
    val lowerPart = heap.take(remained)
    heap.clear() // TODO: better modification method?
    lowerPart.copyToBuffer(heap)
  }

  def resize(width: Int): Unit = keepLow(width + weightLow)

  /** --------
   * info methods
   * -------- */
  def getExactScores(compressor: CompressorGenerator,
                     columnIndex: Int,
                     shouldPipeline: Boolean) = {
    val bitsCoveredShape = compressor.inputFormat // height of columns in input pattern
      .zip(heights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 } // overlap
    val bits = bitsCoveredShape.sum
    val cost = compressor.clbCost(shouldPipeline)
    val reductionEfficiency = (bits - compressor.outputBitsCount) / cost // (bitsIn - bitsOut) / cost
    val reductionRatio = bits.toDouble / compressor.outputBitsCount
    val bitReduction = bits - compressor.outputBitsCount
    val heightReduction = bitsCoveredShape.max - compressor.outputFormat.max
    CompressorScores(bitReduction, heightReduction, reductionEfficiency, reductionRatio)
  }

  /** --------
   * impl soft methods
   * -------- */
  // evaluation
  def eval(add: (T, T) => T, shift: (T, Int) => T, zero: T): T = {
    val temp = heap.map(_.fold(zero)(add))
      .zipWithIndex.map { case (num, index) => shift(num, index) }
      .fold(zero)(add)
    shift(temp, weightLow)
  }

  def evalBigInt: BigInt = {
    def add(a: BigInt, b: BigInt) = a + b

    def shift(a: BigInt, shift: Int) = a << shift

    asSoft.eval(add, shift, BigInt(0))
  }

  def dSoft(): BitHeap[T] = {
    time += 1
    this
  }

  def implStepSoft(stepSolution: CompressorStepSolution) = {
    val heapIn = asSoft.getSub(stepSolution.getCompressor.inputFormat, stepSolution.columnIndex)
    val heapOut = stepSolution.getCompressor.compress(heapIn)
    heapOut
  }

  def implStageSoft(stageSolution: CompressorStageSolution) = {
    val heapOuts: Seq[BitHeap[BigInt]] = stageSolution.compressorSolutions
      .map(implStepSoft)
    val heapNext: BitHeap[BigInt] = heapOuts.reduce(_ + _)
    asSoft.absorbHeapFrom(heapNext)
    this
  }

  def implAllSoft(solution: CompressorFullSolution) = {
    solution.stageSolutions.foreach { StageSolution =>
      asSoft.implStageSoft(StageSolution)
    }
    this
  }

  /** --------
   * impl hard methods, all these methods are in-place
   * -------- */

  def dHard() = {
    asHard.heap.foreach { column => column.indices.foreach(i => column(i) = column(i).d()) }
    this
  }

  def implStageHard(stageSolution: CompressorStageSolution) = {
    val heapOuts: Seq[BitHeap[Bool]] = stageSolution.compressorSolutions
      .map { solution =>
        val heapIn = asHard.getSub(solution.getCompressor.inputFormat, solution.columnIndex)
        val retHeap = solution.getCompressor.compress(heapIn.heap)
        BitHeap(retHeap, heapIn.weightLow, heapIn.time)
      }
    val heapNext: BitHeap[Bool] = heapOuts.reduce(_ + _) // combine all heaps generated
    asHard.absorbHeapFrom(heapNext) // merge the heap generated and the heap remained
    if (stageSolution.pipelined) this.dHard() else this
  }

  def implAllHard(solution: CompressorFullSolution) = {
    solution.stageSolutions.foreach { StageSolution => asHard.implStageHard(StageSolution) }
    this
  }

  // weightLow is not considered
  def toUInts: Seq[UInt] = {
    val ret = ArrayBuffer[UInt]()
    while (heap.exists(_.nonEmpty)) {
      ret += asHard.heap.map(col => if (col.nonEmpty) col.remove(0) else False).asBits().asUInt
    }
    ret
  }

  override def toString = {
    heights.padTo(width, 0)
      .map(columnHeight => Seq.fill(columnHeight)(s"$dot").padTo(heightMax, " "))
      .reverse
      .transpose
      .map(_.mkString(" "))
      .mkString("\n")
  }
}

object BitHeap {

  private def seq2buffer[T](seq: Seq[Seq[T]]): ArrayBuffer[ArrayBuffer[T]] = {
    val buffer = ArrayBuffer.fill(seq.length)(ArrayBuffer[T]())
    seq.zip(buffer).foreach { case (seq, buf) => seq.copyToBuffer(buf) }
    buffer
  }

  /** --------
   * for GPCs
   -------- */

  def fromTable(table: Seq[Seq[BigInt]], weightLow: Int, time: Int): BitHeap[BigInt] =
    BitHeap(seq2buffer(table), weightLow, time)

  def fromHeights(heights: Seq[Int], weightLow: Int, time: Int) = {
    val table = heights.map(height => Seq.fill(height)(BigInt(1, Random)))
    fromTable(table, weightLow, time)
  }

  /** --------
   * for adders
   -------- */

  private def fromOperands[T, Operand](weightedOperands: Seq[(ArithInfo, Operand)], split: (ArithInfo, Operand) => Seq[T]): BitHeap[T] = {
    require(weightedOperands.forall(_._1.isPositive)) // bitheap can't deal with negative bits
    val time = weightedOperands.head._1.time
    val low = weightedOperands.map(_._1.low).min
    require(weightedOperands.forall(_._1.time == time), s"times = ${weightedOperands.map(_._1.time).mkString(" ")}")
    val width = weightedOperands.map(_._1.high).max + 1 - low
    val heap = ArrayBuffer.fill(width)(ArrayBuffer[T]())
    weightedOperands.foreach { case (arithInfo, operand) =>
      val bits = split(arithInfo, operand)
      heap
        .drop(arithInfo.low - low)
        .zip(bits)
        .foreach { case (a, b) => a += b }
    }
    BitHeap(heap, low, time)
  }

  def fromUInts(weightedUInts: Seq[WeightedUInt]): BitHeap[Bool] = {
    def split(arithInfo: ArithInfo, operand: UInt) = operand.asBools

    fromOperands(weightedUInts.map(wU => (wU.arithInfo, wU.value)), split)
  }

  def fromBigInts(weightedBigInts: Seq[WeightedBigInt]): BitHeap[BigInt] = {
    def split(arithInfo: ArithInfo, operand: BigInt) = operand.toBitValue(arithInfo.width).asBools

    fromOperands(weightedBigInts.map(wB => (wB.arithInfo, wB.value)), split)
  }

  def fromInfos(arithInfos: Seq[ArithInfo]) = fromBigInts(arithInfos.map(WeightedBigInt(BigInt(0), _)))
}