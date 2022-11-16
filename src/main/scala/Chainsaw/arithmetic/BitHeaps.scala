package Chainsaw.arithmetic

import spinal.core._

import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.language.postfixOps
import scala.math._
import scala.util.control._
import org.json4s._
import org.json4s.jackson.Serialization._
import BitHeaps._

import Chainsaw._
import Chainsaw.xilinx._

/** @tparam T
  *   a bit heap can be initialized by a non-hardware type, so it can be run outside a component
  * @param bitHeap
  *   the bits, each array buffer in the table stands for a column, low to high
  * @example
  *   bitHeap.head(m)(n) is the (n+1)-th bit of (m+1)-th column in first sub-bitHeap
  * @param weightLow
  *   the base weight of the whole bit heap, this is necessary as a bit matrices can merge with each other
  * @param time
  *   the delay of each sub-bitHeap
  */
case class BitHeapConfigInfo[T](bitHeap: ArrayBuffer[ArrayBuffer[T]], weightLow: Int, time: Int) {
  def +(that: BitHeapConfigInfo[T]): BitHeapConfigInfo[T] = {
    require(time == that.time, "add two BitHeap ConfigInfo must have same time value !")
    val newLow   = weightLow min that.weightLow
    val newHigh  = (weightLow + bitHeap.length - 1) max (that.weightLow + that.bitHeap.length - 1)
    val newWidth = newHigh + 1 - newLow

    // initialization
    val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
    // move bits
    newTable
      .drop(weightLow - newLow) // align
      .zip(bitHeap)
      .foreach { case (a, b) => a ++= b } // move bits
    newTable
      .drop(that.weightLow - newLow) // align
      .zip(that.bitHeap)
      .foreach { case (a, b) => a ++= b } // move bits
    BitHeapConfigInfo(newTable, newLow, time)
  }
}

/** Storing information of a bit matrix(heap), while providing util methods, making operations on bit matrix easier
  *
  * @tparam T
  *   a bit heap can be initialized by a non-hardware type, so it can be run outside a component
  * @param bitHeapConfigInfo
  *   the config information of bitHeap
  * @see
  *   [[BitHeapCompressor]] for hardware implementation
  * @see
  *   ''Arithmetic core generation using bit heaps.'' [[https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6645544]]
  */
case class BitHeaps[T](bitHeapConfigInfo: BitHeapConfigInfo[T]*) {
  val newBitHeapConfigInfos: Seq[BitHeapConfigInfo[T]] = bitHeapConfigInfo.groupBy(_.time).toSeq.map { case (_, infos) => infos.reduce(_ + _) }

  /** -------- attributes
    * --------
    */
  val bitHeaps   = ArrayBuffer(newBitHeapConfigInfos).flatten.map(_.bitHeap)
  val weightLows = ArrayBuffer(newBitHeapConfigInfos).flatten.map(_.weightLow)
  val times      = ArrayBuffer(newBitHeapConfigInfos).flatten.map(_.time)

  // merge two bit heaps
  def +(that: BitHeaps[T]): BitHeaps[T] = {
    require(
      this.lastPipeline == that.lastPipeline,
      s"two BitHeap which will to be add should have the same lastPipeline.\tleftHeap's lastPipeline is ${this.lastPipeline}, rightHeap's lastPipeline is ${that.lastPipeline}"
    )
    val leftHeap  = this.copy
    val rightHeap = that.copy
    rightHeap.times.foreach { t =>
      if (leftHeap.times.contains(t)) {
        val retIndex    = leftHeap.times.indexOf(t)
        val sourceIndex = rightHeap.times.indexOf(t)
        // get size of the new table
        val newLow   = leftHeap.weightLows(retIndex) min rightHeap.weightLows(sourceIndex)
        val newHigh  = leftHeap.weightHighs(retIndex) max rightHeap.weightHighs(sourceIndex)
        val newWidth = newHigh + 1 - newLow

        // initialization
        val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
        // move bits
        newTable
          .drop(leftHeap.weightLows(retIndex) - newLow) // align
          .zip(leftHeap.bitHeaps(retIndex))
          .foreach { case (a, b) => a ++= b } // move bits
        newTable
          .drop(rightHeap.weightLows(sourceIndex) - newLow) // align
          .zip(rightHeap.bitHeaps(sourceIndex))
          .foreach { case (a, b) => a ++= b } // move bits
        leftHeap.bitHeaps(retIndex)   = newTable
        leftHeap.weightLows(retIndex) = newLow
        leftHeap.times(retIndex)      = t
      } else {
        // append new bitHeap
        val sourceIndex = rightHeap.times.indexOf(t)
        leftHeap.bitHeaps += rightHeap.bitHeaps(sourceIndex)
        leftHeap.weightLows += rightHeap.weightLows(sourceIndex)
        leftHeap.times += rightHeap.times(sourceIndex)
      }
    }
    val retHeap = leftHeap.copy
    retHeap
  }

  // add a row on bit heap, in-place operation, the row must be aligned this bit heap
  def addConstant(row: Seq[T], time: Int, zero: T) = {
    require(times.contains(time), s"The source BitHeap isn't contain the delay : $time, please check it !")
    val retIndex = times.indexOf(time)
    val overflow = row.length - widths(retIndex)
    if (overflow > 0) bitHeaps(retIndex) ++= Seq.fill(overflow)(ArrayBuffer[T]())
    bitHeaps(retIndex).zip(row).foreach { case (column, bit) => if (bit != zero) column += bit }
    this
  }

  // x % (BigInt(1) << widthTake)
  def takeLow(widthTake: Int): BitHeaps[T] =
    BitHeaps.getHeapFromTable(bitHeaps.zip(weightLows).map { case (bits, wl) => bits.take(widthTake - wl) }, Seq(widthTake), times)

  // approximately = x / (BigInt(1) << widthDrop)
  def dropLow(widthDrop: Int): BitHeaps[T] =
    BitHeaps.getHeapFromTable(bitHeaps.zip(weightLows).map { case (bits, wl) => bits.drop(widthDrop - wl) }, Seq(widthDrop), times)

  def d(pipeline: T => T): BitHeaps[T] = {
    val pipelinedHeap = BitHeaps(bitHeaps.zip(weightLows.zip(times)).map { case (bitHeap, (weightLow, time)) => BitHeapConfigInfo(bitHeap.map(_.map(pipeline)), weightLow, time) }: _*) // pipeline the whole bit heap
    pipelinedHeap.lastPipeline = lastPipeline
    pipelinedHeap
  }

  def heights = bitHeaps.map(_.map(_.length).toSeq)

  def height = bitHeaps.map(_.map(_.length).max).max // height of the bit heap, compression ends when height is under some bound

  def widths = bitHeaps.map(_.length) // number of columns in each bit heap

  def width = widths.max

  def weightHighs = weightLows.zip(widths).map { case (wgt, wd) => wgt + wd - 1 }

  def bitsCount = bitHeaps.map(_.map(_.length).sum).sum

  def weightLow = weightLows.min

  def maxValue = heights.zip(weightLows).map { case (height, low) => height.zipWithIndex.map { case (count, weight) => (BigInt(1) << (weight + low - weightLow)) * count }.sum }.sum

  def isEmpty = bitHeaps.forall(_.forall(_.isEmpty))

  def currentTime = times.min

  // find the available bitHeap at current time
  def currentIndex = times.indexWhere(_ == currentTime)

  def currentBitHeap = bitHeaps(currentIndex)

  def currentHeights = heights(currentIndex)

  def currentHeight = currentHeights.max

  def currentWidth = widths(currentIndex)

  def currentWeightLow = weightLows(currentIndex)

  def currentHigh = currentWeightLow + currentWidth - 1

  def currentIsEmpty = currentBitHeap.forall(_.isEmpty)

  def currentBitCount = currentHeights.sum

  def currentMaxValue = currentHeights.zipWithIndex.map { case (count, weight) => (BigInt(1) << weight) * count }.sum

  def finalStage = currentTime == times.max && currentHeight <= 6

  def isPipeline = if (finalStage) !lastPipeline else lastPipeline

  var lastPipeline = true

  val carryRecords = Map[Int, Int]()

  /** -------- methods for compression-------- */
  // these methods are designed for compress tree implement
  def implCompressOnce(compressors: Seq[Compressor], compressorSolution: CompressorSolution, isPipeline: Boolean): BitHeaps[Bool] = {
    val compressorInSolution = compressors.find(_.name == compressorSolution.compressorName).get
    val inputTable = compressorInSolution
      .inputFormat(compressorSolution.width)                   // remove and get bits in each columns that you need
      .zip(currentBitHeap.drop(compressorSolution.startIndex)) // align and zip
      .map { case (number, column) =>
        val exactNumber = column.length min number
        val slice       = column.take(exactNumber) // take the bits need
        column --= slice // remove them from current heap
        slice
      }
    val heapIn = BitHeaps.getHeapFromTable(
      Seq(inputTable.asInstanceOf[Seq[ArrayBuffer[Bool]]]),
      Seq(compressorSolution.startIndex + currentWeightLow),
      Seq(if (isPipeline) currentTime + 1 else currentTime)
    )
    val heapOut = compressorInSolution.impl(heapIn, compressorSolution.width)
    heapOut.lastPipeline = lastPipeline
    heapOut
  }

  def implCompressOneStage(compressors: Seq[Compressor], stageSolution: StageSolution, pipeline: Bool => Bool): BitHeaps[Bool] = {
    val results = ArrayBuffer[BitHeaps[Bool]]()
    stageSolution.compressorSolutions.foreach(compressorSolution => results += implCompressOnce(compressors, compressorSolution, stageSolution.isPipeline))
    val partialNextStage = results.reduce(_ + _).d(if (stageSolution.isPipeline) pipeline else b => b)
    this.bitHeaps.remove(currentIndex)
    this.weightLows.remove(currentIndex)
    this.times.remove(currentIndex)
    val nextStageHeap = this.asInstanceOf[BitHeaps[Bool]] + partialNextStage
    nextStageHeap.lastPipeline = stageSolution.isPipeline
    require(nextStageHeap.toString == stageSolution.getNextBitHeap.toString, s"hard:\n${nextStageHeap.toString}\nsoft:\n${stageSolution.getNextBitHeap.toString}")
    nextStageHeap
  }

  def implCompressTree(compressors: Seq[Compressor], compressTreeSolution: CompressTreeSolution, pipeline: Bool => Bool, name: String): BitHeaps[Bool] = {
    logger.info(s"begin to implement the hardware compress tree of $name")
    var currentHeap = this
    compressTreeSolution.solutions.foreach(stageSolution => currentHeap = currentHeap.implCompressOneStage(compressors, stageSolution, pipeline).asInstanceOf[BitHeaps[T]])
    currentHeap.asInstanceOf[BitHeaps[Bool]]
  }

  /** get the exact(rather than maximum) efficiency of a compressor applied on current bit heap
    *
    * @param columnIndex
    *   index of column with lowest weight covered by the compressor in heights
    */
  def getExactReductionEfficiency(
      compressor: Compressor,
      considerCarry8: Boolean,
      width: Int,
      columnIndex: Int
  ): Double = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }    // overlap
      .sum

    (bits -                                                  // bitsIn
      compressor.outputBitsCount(width)) /                   // bitsOut
      compressor.areaCost(width, considerCarry8, isPipeline) // divided by cost
  }

  def getExactReductionRatio(
      compressor: Compressor,
      width: Int,
      columnIndex: Int
  ): Double = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }
      .sum // overlap

    bits.toDouble / compressor.outputBitsCount(width)
  }

  def getExactBitReduction(
      compressor: Compressor,
      width: Int,
      columnIndex: Int
  ): Int = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }
      .sum // overlap

    bits - compressor.outputBitsCount(width)
  }

  def getExactHeightReduction(
      compressor: Compressor,
      width: Int,
      columnIndex: Int
  ): Int = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }

    bits.max - compressor.outputFormat(width).max
  }

  def isRedundant(
      compressor: Compressor,
      width: Int,
      columnIndex: Int
  ): Boolean = {
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }

    val inputMaxValue = bits.zipWithIndex.map { case (bit, w) => (BigInt(1) << w) * bit }.sum
    val outputMaxValue = compressor
      .outputFormat(width)
      .zipWithIndex
      .map { case (bit, w) => (BigInt(1) << w) * bit }
      .sum
    outputMaxValue > inputMaxValue
  }

  def headStrategy(
      compressors: Seq[Compressor],
      considerCarry8: Boolean,
      headBound: Double,
      useHeightControlStrategy: Boolean
  ) = {
    val searchThreshold = 0.2
    val effBound        = headBound

    val columnIndex    = currentHeights.indexWhere(_ == currentHeights.max) // find the first(lowest weight) column with maximum height
    var bestCompressor = compressors.head
    var bestWidth      = 1
    var bestReductionEfficiency =
      bestCompressor.reductionEfficiency(bestWidth, considerCarry8)
    var bestReductionRatio = bestCompressor.reductionRatio(bestWidth)

    val candidates = compressors.tail
      .sortBy(compressor => compressor.reductionEfficiency((currentWidth - columnIndex) min compressor.widthMax, considerCarry8))
      .reverse

    val hasNextTimeHeap                           = times.contains(currentTime + 1)
    val nextHeights                               = if (hasNextTimeHeap) bitHeaps(times.indexOf(currentTime + 1)).map(_.length) else ArrayBuffer[Int]()
    val nextWeightLow                             = if (hasNextTimeHeap) weightLows(times.indexOf(currentTime + 1)) else 0
    var nextHeapCurrentHeight, nextHeapNextHeight = 0
    if (hasNextTimeHeap) {
      val indexInNextHeap = currentWeightLow + columnIndex - nextWeightLow
      if (indexInNextHeap == -1) nextHeapNextHeight = nextHeights(0)
      if (indexInNextHeap >= 0) {
        if (indexInNextHeap <= nextHeights.length - 2) {
          nextHeapCurrentHeight = nextHeights(indexInNextHeap)
          nextHeapNextHeight    = nextHeights(indexInNextHeap + 1)
        }
        if (indexInNextHeap == nextHeights.length - 1) {
          nextHeapCurrentHeight = nextHeights(indexInNextHeap)
        }
      }
    }
    var abnormalCompress = false

    if (currentHeights(columnIndex) == 2 && columnIndex <= currentWidth - 2) {
      abnormalCompress = (currentHeights(columnIndex) + carryRecords.getOrElse(columnIndex, 0) + nextHeapCurrentHeight >= 3 && currentHeights(columnIndex) + carryRecords.getOrElse(columnIndex, 0)
        + nextHeapCurrentHeight <= currentHeights(columnIndex + 1) + carryRecords.getOrElse(columnIndex + 1, 0) + nextHeapNextHeight) ||
        (currentHeights(columnIndex) + carryRecords.getOrElse(columnIndex, 0) + nextHeapCurrentHeight < 3 && currentHeights(columnIndex + 1) + carryRecords.getOrElse(columnIndex + 1, 0) + nextHeapNextHeight >= 3)
    }

    if ((!abnormalCompress && useHeightControlStrategy) || !useHeightControlStrategy) {
      candidates.foreach { compressor =>
        val widthMax = compressor.widthMax min (currentWidth - columnIndex)
        if (compressor.isFixed) {
          val exactReductionEfficiency = getExactReductionEfficiency(compressor, considerCarry8, width = -1, columnIndex)
          val exactReductionRatio      = getExactReductionRatio(compressor, width = -1, columnIndex = columnIndex)
          if (exactReductionEfficiency >= (bestReductionEfficiency max effBound)) {
            if (exactReductionEfficiency == bestReductionEfficiency) {
              if (exactReductionRatio >= bestReductionRatio) {
                bestReductionEfficiency = exactReductionEfficiency
                bestWidth               = -1
                bestCompressor          = compressor
                bestReductionRatio      = exactReductionRatio
              }
            } else {
              bestReductionEfficiency = exactReductionEfficiency
              bestWidth               = -1
              bestCompressor          = compressor
              bestReductionRatio      = exactReductionRatio
            }
          }
        } else {
          if (widthMax >= compressor.widthMin) {
            var searchWidth = widthMax
            while (searchWidth >= compressor.widthMin) {
              val exactReductionEfficiency = getExactReductionEfficiency(compressor, considerCarry8, width = searchWidth, columnIndex)
              val exactReductionRatio      = getExactReductionRatio(compressor, searchWidth, columnIndex)
              if (exactReductionEfficiency >= (bestReductionEfficiency max effBound)) {
                if (exactReductionEfficiency == bestReductionEfficiency) {
                  if (exactReductionRatio >= bestReductionRatio) {
                    bestReductionEfficiency = exactReductionEfficiency
                    bestWidth               = searchWidth
                    bestCompressor          = compressor
                    bestReductionRatio      = exactReductionRatio
                  }
                } else {
                  bestReductionEfficiency = exactReductionEfficiency
                  bestWidth               = searchWidth
                  bestCompressor          = compressor
                  bestReductionRatio      = exactReductionRatio
                }
              }
              if ((exactReductionEfficiency + searchThreshold) < effBound) {
                searchWidth = 8 * floor((searchWidth - 1) / 8).toInt
              } else
                searchWidth -= 1
            }
          }
        }
        if (bestCompressor.outputFormat(bestWidth).length > 1) {
          bestCompressor.outputFormat(bestWidth).zipWithIndex.foreach { case (h, i) =>
            val oldRecord = carryRecords.get(columnIndex + i)
            oldRecord match {
              case Some(old) =>
                carryRecords.remove(columnIndex + i)
                carryRecords.put(columnIndex + i, old + h)
              case None =>
                carryRecords.put(columnIndex + i, h)
            }
          }
        }
      }
    }
    (bestCompressor, bestWidth, columnIndex, bestReductionEfficiency)
  }

  def tailStrategy(
      compressors: Seq[Compressor],
      considerCarry8: Boolean,
      tailBound: Double,
      useHeightControlStrategy: Boolean
  ) = {

    val effBound                = tailBound
    var bestCompressor          = compressors.head
    var bestWidth               = 1
    var bestReductionEfficiency = bestCompressor.reductionEfficiency(bestWidth, considerCarry8, isPipeline)
    var bestHeightReduction     = 0

    val columnIndex = currentHeights.indexWhere(_ == currentHeights.max)
    val candidates = compressors.tail
      .filter(_.isFixed)
      .sortBy(compressor => compressor.reductionEfficiency(width = -1, considerCarry8, isPipeline))
      .reverse

    val formalCompress =
      if (columnIndex <= currentWidth - 2) {
        if (!isPipeline)
          currentHeights(columnIndex) == 2 && carryRecords.getOrElse(columnIndex, 0) == 0 && currentHeights(columnIndex + 1) + carryRecords.getOrElse(columnIndex + 1, 0) == 2 ||
          currentHeights(columnIndex) + carryRecords.getOrElse(columnIndex, 0) > 3
        else
          currentHeights(columnIndex) == 2 && carryRecords.getOrElse(columnIndex, 0) > 0
      } else false

    if (useHeightControlStrategy) {
      if (currentHeights(columnIndex) > 2 || formalCompress) {
        candidates.foreach { compressor =>
          val exactReductionEfficiency = getExactReductionEfficiency(compressor, considerCarry8, width = -1, columnIndex)
          val exactHeightReduction     = getExactHeightReduction(compressor, width = -1, columnIndex)
          if (exactHeightReduction >= bestHeightReduction) {
            if (exactHeightReduction == bestHeightReduction) {
              if (exactReductionEfficiency >= (bestReductionEfficiency max effBound)) {
                bestReductionEfficiency = exactReductionEfficiency
                bestCompressor          = compressor
                bestWidth               = -1
                bestHeightReduction     = exactHeightReduction
              }
            } else {
              bestReductionEfficiency = exactReductionEfficiency
              bestCompressor          = compressor
              bestWidth               = -1
              bestHeightReduction     = exactHeightReduction
            }
          }
        }
        if (bestCompressor.outputFormat(bestWidth).length > 1) {
          bestCompressor.outputFormat(bestWidth).zipWithIndex.foreach { case (h, i) =>
            val oldRecord = carryRecords.get(columnIndex + i)
            oldRecord match {
              case Some(old) =>
                carryRecords.remove(columnIndex + i)
                carryRecords.put(columnIndex + i, old + h)
              case None =>
                carryRecords.put(columnIndex + i, h)
            }
          }
        }
      }
    } else {
      candidates.foreach { compressor =>
        val exactReductionEfficiency = getExactReductionEfficiency(compressor, considerCarry8, width = -1, columnIndex)
        val exactHeightReduction     = getExactHeightReduction(compressor, width = -1, columnIndex)
        if (exactHeightReduction >= bestHeightReduction) {
          if (exactHeightReduction == bestHeightReduction) {
            if (exactReductionEfficiency >= (bestReductionEfficiency max effBound)) {
              bestReductionEfficiency = exactReductionEfficiency
              bestCompressor          = compressor
              bestWidth               = -1
              bestHeightReduction     = exactHeightReduction
            }
          } else {
            bestReductionEfficiency = exactReductionEfficiency
            bestCompressor          = compressor
            bestWidth               = -1
            bestHeightReduction     = exactHeightReduction
          }
        }
      }
      if (bestCompressor.outputFormat(bestWidth).length > 1) {
        bestCompressor.outputFormat(bestWidth).zipWithIndex.foreach { case (h, i) =>
          val oldRecord = carryRecords.get(columnIndex + i)
          oldRecord match {
            case Some(old) =>
              carryRecords.remove(columnIndex + i)
              carryRecords.put(columnIndex + i, old + h)
            case None =>
              carryRecords.put(columnIndex + i, h)
          }
        }
      }
    }

    (bestCompressor, bestWidth, columnIndex, bestReductionEfficiency)
  }

  /** get the most efficient compressor for current bit heap and do compression with it
    *
    * @param compressors
    *   a list of available compressors, the first one must be a 1 to 1 compressor which won't do compression
    * @return
    *   new bit heap generated by the compressor, and the LUT cost
    */
  def compressOneTime(
      compressors: Seq[Compressor],
      considerCarry8: Boolean,
      headBound: Double,
      tailBound: Double,
      finalStageBefore: Boolean,
      shouldPipeline: Boolean,
      useHeightControlStrategy: Boolean = true
  ): (BitHeaps[T], CompressorSolution) = {

    val (bestCompressor, bestWidth, columnIndex, bestReductionEfficiency) =
      if (finalStageBefore) tailStrategy(compressors, considerCarry8, tailBound, useHeightControlStrategy)
      else headStrategy(compressors, considerCarry8, headBound, useHeightControlStrategy)
    val inputTable = bestCompressor
      .inputFormat(bestWidth)                // remove and get bits in each columns that you need
      .zip(currentBitHeap.drop(columnIndex)) // align and zip
      .map { case (number, column) =>
        val exactNumber = column.length min number
        val slice       = column.take(exactNumber) // take the bits need
        column --= slice // remove them from current heap
        slice
      }

    val heapIn   = BitHeaps.getHeapFromTable(Seq(inputTable.asInstanceOf[Seq[ArrayBuffer[Bool]]]), Seq(columnIndex + currentWeightLow), Seq(currentTime))
    val heapOut  = getHeapFromHeights(Seq(bestCompressor.outputFormat(bestWidth)), Seq(columnIndex + currentWeightLow), Seq(if (shouldPipeline) currentTime + 1 else currentTime)).asInstanceOf[BitHeaps[T]]
    val areaCost = bestCompressor.areaCost(bestWidth, considerCarry8, shouldPipeline)
    val currentCompressorSolution = CompressorSolution(
      bestCompressor.name,
      bestWidth,
      columnIndex,
      Consideration(areaCost, bestReductionEfficiency, heapIn.bitsCount.toDouble / heapOut.bitsCount, heapIn.bitsCount - heapOut.bitsCount, heapIn.height - heapOut.height)
    )
    heapOut.lastPipeline = lastPipeline
    (heapOut, currentCompressorSolution)
  }

  /** do compression until all bits are covered and go to next stage
    *
    * @return
    *   new heap for the next stage, and the LUT cost
    */
  def compressOneStage(
      compressors: Seq[Compressor],
      considerCarry8: Boolean           = true,
      headBound: Double                 = 1.0,
      tailBound: Double                 = 0.0,
      bitRatioTarget: Double            = 1.0,
      reductionEfficiencyTarget: Double = 1.8,
      finalHeight: Int                  = 2
  ): (BitHeaps[T], StageSolution, String) = {
    require((!finalStage && lastPipeline) || this.finalStage, s"finalStage: ${this.finalStage}\tlastPipeline: ${this.lastPipeline}")
    val finalStageBefore = finalStage
    var shouldPipeline   = isPipeline

    val currentBitCountBefore = currentBitCount
    var currentBitCountAfter  = currentBitCountBefore
    val currentHeightBefore   = currentHeight
    var currentHeightAfter    = currentHeightBefore
    val heightBefore          = height

    var stageAreaCost       = 0.0
    val results             = ArrayBuffer[BitHeaps[T]]()
    val compressorTypes     = Set[String]()
    val compressorSolutions = ArrayBuffer[CompressorSolution]()
    var redundant           = false

    // compress until all bits are covered
    while (!currentIsEmpty) {
      val (heap, compressorSolution) = compressOneTime(compressors, considerCarry8, headBound, tailBound, finalStageBefore, shouldPipeline)
      results += heap
      stageAreaCost += compressorSolution.getAreaCost
      compressorTypes += compressorSolution.compressorName
      compressorSolutions += compressorSolution
      if (isRedundant(compressors.find(_.name == compressorSolution.compressorName).get, compressorSolution.width, compressorSolution.startIndex)) redundant = true
    }

    //    logger.info(results.map(_.lastPipeline.toString).mkString(","))
    val partialNextStage = results
      .asInstanceOf[Seq[BitHeaps[T]]]
      .reduce(_ + _)
      .asInstanceOf[BitHeaps[T]] // when T is Bool

    this.bitHeaps.remove(currentIndex)
    this.weightLows.remove(currentIndex)
    this.times.remove(currentIndex)

    currentBitCountAfter = partialNextStage.bitsCount
    currentHeightAfter   = partialNextStage.height
    val nextStage = this + partialNextStage

    if (nextStage.height <= finalHeight && nextStage.bitHeaps.length <= 1 && !shouldPipeline) {
      shouldPipeline = true
      nextStage.times.indices.foreach(i => nextStage.times(i) += 1)
    }
    nextStage.lastPipeline = shouldPipeline
    val stageBitReduction        = currentBitCountBefore - currentBitCountAfter
    val stageReductionRatio      = currentBitCountBefore.toDouble / currentBitCountAfter
    val stageReductionEfficiency = (currentBitCountBefore - currentBitCountAfter).toDouble / stageAreaCost
    val stageHeightReduction     = currentHeightBefore - currentHeightAfter

    val stageLog = s"compressed info :\n\tstage bit reduction: $stageBitReduction, stage reduction efficiency: $stageReductionEfficiency, stage reduction ratio: $stageReductionRatio" +
      s"\n\tarea cost: $stageAreaCost, height: $currentHeightBefore -> $currentHeightAfter" +
      s"\n\tcompressors used: ${compressorTypes.mkString(",")}" +
      s"\n\twhole info :\n\theight: $heightBefore -> ${nextStage.height}, bits remained: ${nextStage.bitsCount}"

    if (verbose >= 1 && stageReductionRatio >= bitRatioTarget && stageReductionEfficiency >= reductionEfficiencyTarget)
      logger.info(stageLog)
    if (finalStageBefore && shouldPipeline && verbose >= 1 && stageReductionRatio >= bitRatioTarget && stageReductionEfficiency >= reductionEfficiencyTarget)
      logger.info(s"\n${nextStage.toString}")
    (
      nextStage,
      StageSolution(
        compressorSolutions,
        Consideration(stageAreaCost, stageReductionEfficiency, stageReductionRatio, stageBitReduction, stageHeightReduction),
        StageInfo(
          s"$currentHeightBefore -> $currentHeightAfter",
          s"$heightBefore -> ${nextStage.height}",
          Seq.tabulate(nextStage.bitHeaps.length)(i => BitHeapConfigInfo(nextStage.bitHeaps(i).map(_.map(_ => 0)), nextStage.weightLows(i), nextStage.times(i))),
          finalStageBefore,
          shouldPipeline,
          redundant
        )
      ),
      stageLog
    )
  }

  /** do compression until there's no more than two lines in the bit heap
    *
    * @return
    *   final bit heap and the key information of the compressor tree (latency, widthOut, etc.)
    */
  def compressAll(
      candidates: Seq[Compressor],
      considerCarry8: Boolean      = true,
      name: String                 = "compressor tree of temp",
      useHistorySolutions: Boolean = false,
      finalHeight: Int             = 2
  ): (BitHeaps[T], CompressTreeSolution) = {

    if (verbose >= 1) {
      if (name != null) logger.info(s"the name of compressor tree is : $name")
      logger.info(
        s"\n----available compressors----"
          + s"\n\t${candidates.map(compressor => compressor.getClass.getSimpleName.init + "\n" + compressor.toString(8)).mkString("\n\t")}"
      )
      logger.info(s"initial state: bits in total: $bitsCount, height: $height")
      if (finalStage) logger.info(s"initial enter finalStage")
      logger.info(s"\n$toString")
    }

    implicit val formats: DefaultFormats.type = DefaultFormats
    val solutionsFile =
      new File(compressorSolutionDir, s"${this.toString.hashCode()}.txt")
    if (solutionsFile.exists() && useHistorySolutions) {
      val solutionsReader = new BufferedReader(new FileReader(solutionsFile))
      val compressTreeSolution =
        read[CompressTreeSolution](solutionsReader.readLine())
      logger.info(s"Find a history solution in path: $compressorSolutionDir/${this.toString.hashCode()}.txt, load this solution.")
      compressTreeSolution.printLog(srcBitHeap = this.asInstanceOf[BitHeaps[Int]])
      (
        if (compressTreeSolution.getFinalBitHeap != null)
          compressTreeSolution.getFinalBitHeap.asInstanceOf[BitHeaps[T]]
        else this,
        compressTreeSolution
      )
    } else {
      val bitsInTotal                   = this.bitsCount
      val maxValue                      = this.maxValue
      var current                       = this
      var latency                       = 0
      var badLatency                    = 0
      var allCost                       = 0.0
      val stageSolutions                = ArrayBuffer[StageSolution]()
      val initHeadEfficiencyBound       = 1.0
      val initTailEfficiencyBound       = 0.5
      var headEfficiencyBound           = initHeadEfficiencyBound
      var tailEfficiencyBound           = initTailEfficiencyBound
      val bitRatioTargetList            = List(2.0, 1.0)
      val reductionEfficiencyTargetList = List(1.9, 0.5)
      var bitRatioTarget                = bitRatioTargetList.head
      var reductionEfficiencyTarget     = reductionEfficiencyTargetList.head

      val candidateStageSolutions = ArrayBuffer[StageSolution]()
      val candidateBitHeaps       = ArrayBuffer[BitHeaps[T]]()
      val candidateStageLogs      = ArrayBuffer[String]()
      val candidateHeadBounds     = ArrayBuffer[Double]()
      val candidateTailBounds     = ArrayBuffer[Double]()
      while ((current.height > finalHeight || current.bitHeaps.length > 1) && latency < 100) {
        val currentBefore = current.copy
        bitRatioTarget =
          if (currentBefore.finalStage) bitRatioTargetList.last
          else bitRatioTargetList.head
        reductionEfficiencyTarget =
          if (currentBefore.finalStage) reductionEfficiencyTargetList.last
          else reductionEfficiencyTargetList.head
        val (heap, stageSolution, stageLog) =
          current.compressOneStage(
            candidates,
            considerCarry8,
            headEfficiencyBound,
            tailEfficiencyBound,
            bitRatioTarget,
            reductionEfficiencyTarget
          )
        if (stageSolution.getReductionRatio >= bitRatioTarget && stageSolution.getReductionEfficiency >= reductionEfficiencyTarget) {
          headEfficiencyBound = initHeadEfficiencyBound
          tailEfficiencyBound = initTailEfficiencyBound
          current             = heap
          allCost += stageSolution.getAreaCost
          if (stageSolution.isPipeline) latency += 1
          stageSolutions += stageSolution
          if (currentBefore.finalStage && stageSolution.isPipeline)
            badLatency += 1
          if (verbose >= 1 && !currentBefore.finalStage && current.finalStage)
            logger.info(s"enter finalStage")
          candidateStageSolutions.clear()
          candidateBitHeaps.clear()
          candidateStageLogs.clear()
          candidateHeadBounds.clear()
          candidateTailBounds.clear()
        } else {
          candidateStageSolutions += stageSolution
          candidateBitHeaps += heap
          candidateStageLogs += stageLog
          candidateHeadBounds += headEfficiencyBound
          candidateTailBounds += tailEfficiencyBound
          if (headEfficiencyBound > 0.2 && tailEfficiencyBound > 0.2) {
            if (currentBefore.finalStage) tailEfficiencyBound -= 0.1
            else headEfficiencyBound -= 0.1
            current = currentBefore
          } else {
            headEfficiencyBound = initHeadEfficiencyBound
            tailEfficiencyBound = initTailEfficiencyBound
            val (finalStageSolution, (finalHeap, finalStageLog)) =
              candidateStageSolutions
                .zip(candidateBitHeaps.zip(candidateStageLogs))
                .sortBy(_._1.getReductionEfficiency)
                .reverse
                .head
            allCost += finalStageSolution.getAreaCost
            if (finalStageSolution.isPipeline) latency += 1
            stageSolutions += finalStageSolution
            current = finalHeap
            if (verbose >= 1) logger.info(finalStageLog)
            if (verbose >= 1 && currentBefore.finalStage && finalStageSolution.isPipeline) {
              badLatency += 1
              logger.info(s"\n${finalHeap.toString}")
            }
            if (verbose >= 1 && !currentBefore.finalStage && current.finalStage)
              logger.info(s"enter finalStage")
            candidateStageSolutions.clear()
            candidateBitHeaps.clear()
            candidateStageLogs.clear()
            candidateHeadBounds.clear()
            candidateTailBounds.clear()
          }
        }
      }
      val allCompressed = bitsInTotal - current.bitsCount
      logger.info(
        s"\n----efficiency report of bit heap compressor----" +
          s"\n\tcost in total: $allCost, compressed in total: $allCompressed" +
          s"\n\tefficiency in total: ${allCompressed.toDouble / allCost}" +
          s"\n\tideal widthOut: ${maxValue.bitLength}, actual widthOut: ${current.widths.max}" +
          s"\n\t${if (stageSolutions.exists(_.isRedundant)) "has redundant compressor"
          else "all compressor isn't redundant"}" +
          s"\n\t${if (current.widths.max > maxValue.bitLength) "output is redundant, need to be resized"
          else "output isn't redundant"}"
      )

      val compressTreeSolution = CompressTreeSolution(stageSolutions)
      val solutionsWriter      = new BufferedWriter(new FileWriter(solutionsFile))
      solutionsWriter.write(write(compressTreeSolution))
      solutionsWriter.flush()
      solutionsWriter.close()
      logger.info(s"Store a solution to path : ${compressorSolutionDir.getAbsolutePath}/${this.toString.hashCode()}.txt")

      (current, compressTreeSolution)
    }

  }

  def output(zero: () => T, finalHeight: Int = 2): ArrayBuffer[ArrayBuffer[T]] = {
    require(height <= finalHeight && bitHeaps.length <= 1, s"Output style illegal, height: $height\tnumber of delayed bitHeap: ${bitHeaps.length}")
    val resizedBitHeap =
      if (width > maxValue.bitLength) bitHeaps.head.take(maxValue.bitLength)
      else bitHeaps.head
    resizedBitHeap.map(_.padTo(finalHeight, zero())).transpose
  }

  override def toString = {
    val dotDiagram = ArrayBuffer[String]()
    bitHeaps.zip(times.zip(weightLows)).foreach { case (bitHeap, (time, weightLow)) =>
      val singleBitHeap =
        BitHeaps(BitHeapConfigInfo(bitHeap, weightLow, time))
      val width   = log2Up(singleBitHeap.maxValue)
      val heights = singleBitHeap.heights.head
      dotDiagram += s"time = $time ; weightLow = $weightLow\n" + heights
        .padTo(width, 0)
        .map(columnHeight => Seq.fill(columnHeight)(s"$dot").padTo(singleBitHeap.height, " "))
        .reverse
        .transpose
        .map(_.mkString(" "))
        .mkString("\n")
    }
    dotDiagram.mkString("\n")
  }

  // deep copy
  def copy: BitHeaps[T] = {
    val copyRet = BitHeaps(this.bitHeaps.map(_.map(_.map(i => i))), this.weightLows, this.times)
    copyRet.lastPipeline = this.lastPipeline
    copyRet
  }

}

object BitHeaps {
  def getHeapFromTable[T](
      tables: Seq[Seq[Seq[T]]],
      weightLows: Seq[Int],
      times: Seq[Int]
  ): BitHeaps[T] = {
    val bitHeapConfigInfos = ArrayBuffer[BitHeapConfigInfo[T]]()
    times.zip(tables).zip(weightLows).foreach { case ((time, table), weightLow) =>
      val tableForHeap = ArrayBuffer.fill(table.length)(ArrayBuffer[T]())
      tableForHeap.zip(table).foreach { case (buf, seq) => buf ++= seq }
      bitHeapConfigInfos += BitHeapConfigInfo(tableForHeap, weightLow, time)
    }
    BitHeaps(bitHeapConfigInfos: _*)
  }

  /** build bit matrix from operands and their shifts
    *
    * @param infos
    *   infos record the width and position(shift) info of corresponding operands
    * @param operands
    *   an operand is a low to high sequence of bits
    */
  def getHeapFromInfos[T](
      infos: Seq[Seq[ArithInfo]],
      operands: Seq[Seq[Seq[T]]] = null
  ): BitHeaps[T] = {
    val bitHeapConfigInfos = ArrayBuffer[BitHeapConfigInfo[T]]()
    val realOperands =
      if (operands == null)
        infos.map(_.map(info => Seq.fill(info.width)(0.asInstanceOf[T])))
      else operands
    val sortedInfos = infos.flatten
      .zip(realOperands.flatten)
      .groupBy(_._1.time)
      .toSeq
      .map(_._2)
      .map(info => (info.map(_._1), info.map(_._2)))
    sortedInfos.foreach { case (info, operand) =>
      //      require(info.forall(_.time == info.head.time), s"The delay of ${infos.indexOf(info)}th infos is illegal, delay -> ${info.map(_.time).mkString(",")}.")
      // get the width of the table
      val positionHigh = info.map(_.high).max
      val positionLow  = info.map(_.low).min
      val width        = positionHigh - positionLow + 1
      // build the table from operands
      val table = ArrayBuffer.fill(width)(ArrayBuffer[T]())
      operand.zip(info).foreach { case (row, inf) =>
        val start = inf.weight - positionLow
        // insert bits from low to high
        (start until start + inf.width).foreach(i => table(i) += row(i - start))
      }
      bitHeapConfigInfos += BitHeapConfigInfo(table, positionLow, info.head.time)
    }
    BitHeaps(bitHeapConfigInfos: _*)
  }

  def getHeapFromHeights(
      heights: Seq[Seq[Int]],
      weightLows: Seq[Int],
      times: Seq[Int]
  ): BitHeaps[Int] = {
    val bitHeapConfigInfos = ArrayBuffer[BitHeapConfigInfo[Int]]()
    times.zip(heights).zip(weightLows).foreach { case ((time, heights), weightLow) =>
      bitHeapConfigInfos += BitHeapConfigInfo(ArrayBuffer(heights: _*).map(i => ArrayBuffer.fill(i)(0)), weightLow, time)
    }
    BitHeaps(bitHeapConfigInfos: _*)
  }

  def getInfosFromBitHeap[T](bitHeap: BitHeaps[T]): Seq[Seq[ArithInfo]] = {
    val copyHeap     = bitHeap.bitHeaps.map(_.map(_.map(b => b)))
    val infosForHeap = ArrayBuffer[ArrayBuffer[ArithInfo]]()
    bitHeap.times.zip(copyHeap).zip(bitHeap.weightLows).foreach { case ((time, heap), weightLow) =>
      val infos = ArrayBuffer[ArithInfo]()
      while (!heap.forall(_.isEmpty)) {
        val start  = heap.indexOf(heap.find(_.nonEmpty).get)
        val width  = heap.drop(start).span(_.nonEmpty)._1.length
        val weight = start + weightLow
        infos += ArithInfo(width, weight, time = time)
        Range(start, start + width).foreach { idx =>
          heap(idx) -= heap(idx).head
        }
      }
      infosForHeap += infos
    }
    infosForHeap
  }

  def apply[T](bitHeap: ArrayBuffer[ArrayBuffer[T]], weightLow: Int, time: Int): BitHeaps[T] =
    BitHeaps(BitHeapConfigInfo(bitHeap, weightLow, time))

  def apply[T](bitHeaps: Seq[ArrayBuffer[ArrayBuffer[T]]], weightLows: Seq[Int], times: Seq[Int]): BitHeaps[T] = BitHeaps(
    bitHeaps.zip(weightLows.zip(times)).map { case (bitHeap, (weightLow, time)) =>
      BitHeapConfigInfo(bitHeap, weightLow, time)
    }: _*
  )
}
