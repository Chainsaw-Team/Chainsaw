package Chainsaw.arithmetic.bitheap

import Chainsaw.arithmetic.{Compressor3to1, Compressor6to3}
import Chainsaw._
import Chainsaw.arithmetic._

import scala.math._
import scala.collection.mutable.ArrayBuffer

abstract class BitHeapSolver {

  def solverName = className(this)

  /** core method which must be kept
    */
  def solveAll(bitHeapGroup: BitHeapGroup[BigInt]): CompressorFullSolution = {

    bitHeapGroup.absorbConstant()

    val timeAndHeaps: Map[Int, BitHeap[BigInt]] = bitHeapGroup.bitHeaps.groupBy(_.time).map(pair => pair._1 -> pair._2.head)
    val stages                                  = ArrayBuffer[CompressorStageSolution]()

    var currentTime = timeAndHeaps.keys.min
    var currentHeap = timeAndHeaps(currentTime)

    // TODO: allow unpipelined stage
    while (currentTime < timeAndHeaps.keys.max) {
      stages += solveStage(currentHeap)
      if (stages.last.pipelined) {
        timeAndHeaps.get(currentTime + 1) match {
          case Some(heapNext) =>
            heapNext.absorbHeapFrom(currentHeap.dSoft())
            currentHeap = heapNext
          case None =>
            currentHeap = currentHeap.dSoft()
        }
        currentTime += 1
      }
    }

    while (currentHeap.heightMax > 3) stages += solveStage(currentHeap)
    CompressorFullSolution(stages)
  }

  // TODO: remove this and leave the interface in BitHeapGroup only
  def solveAll(bitHeap: BitHeap[BigInt]): CompressorFullSolution = {
    bitHeap.absorbConstant()
    val stages = ArrayBuffer[CompressorStageSolution]()
    while (bitHeap.heightMax > 3) stages += solveStage(bitHeap)
    CompressorFullSolution(stages)
  }

  def solveStage(bitHeap: BitHeap[BigInt]): CompressorStageSolution

  type TestCase = Seq[ArithInfo]

}

object NaiveSolver extends BitHeapSolver {

  def apply(bitHeapGroup: BitHeapGroup[BigInt]): CompressorFullSolution = solveAll(bitHeapGroup)

  def apply(bitHeap: BitHeap[BigInt]): CompressorFullSolution = solveAll(bitHeap)

  override def solveStage(bitHeap: BitHeap[BigInt]): CompressorStageSolution = {

    import bitHeap._

    def isLastStage = heightMax <= 6

    def solveStep: CompressorStepSolution = {
      if (!isLastStage) {
        val columnIdx = heap.indexWhere(_.nonEmpty)
        val endIdx    = heap.lastIndexWhere(_.nonEmpty)
        val width     = ((endIdx - columnIdx + 1) min cpaWidthMax) max 8
        CompressorStepSolution(compressorName = "Compressor3to1", width, columnIdx, getExactScores(Compressor3to1(width), columnIdx, shouldPipeline = true))
      } else {
        val columnIdx = heap.indexWhere(_.nonEmpty)
        CompressorStepSolution(compressorName = "Compressor6to3", width, columnIdx, getExactScores(Compressor6to3(), columnIdx, shouldPipeline = true))
      }
    }

    val steps    = ArrayBuffer[CompressorStepSolution]()
    val heapOuts = ArrayBuffer[BitHeap[BigInt]]()
    while (nonEmpty) {
      val stepSolution = solveStep
      steps += stepSolution
      heapOuts += implStepSoft(stepSolution)
    }
    val heapNext: BitHeap[BigInt] = heapOuts.reduce(_ + _)
    asSoft.absorbHeapFrom(heapNext)
    if (verbose >= 1) logger.info(s"----------------\n$this----------------\n")
    CompressorStageSolution(steps, heightMax, pipelined = true)
  }
}

object GreedSolver extends BitHeapSolver {

  override def solveStage(bitHeap: BitHeap[BigInt]): CompressorStageSolution = {

    import bitHeap._
    def solveStep: CompressorStepSolution = {
      val columnIndex             = heights.indexWhere(_ == heightMax)
      var bestCompressor          = getCompressor("Compressor1to1", 1)
      var bestReductionEfficiency = bestCompressor.reductionEfficiency()
      var bestHeightReduction     = bestCompressor.heightReduction

      val candidates = candidateCompressors.sortBy(compressor => compressor.reductionEfficiency()).reverse

      candidates.foreach {
        case gpc: Gpc =>
          val exactScores = getExactScores(gpc, columnIndex, shouldPipeline = true)
          if (exactScores.reductionEfficiency >= bestReductionEfficiency) {
            if (exactScores.reductionEfficiency == bestReductionEfficiency) {
              if (exactScores.heightReduction >= bestHeightReduction) {
                bestReductionEfficiency = exactScores.reductionEfficiency
                bestCompressor          = gpc
                bestHeightReduction     = exactScores.heightReduction
              }
            } else {
              bestReductionEfficiency = exactScores.reductionEfficiency
              bestCompressor          = gpc
              bestHeightReduction     = exactScores.heightReduction
            }
          }
        case rowAdder: RowAdder =>
          val searchWidthMax = rowAdder.widthMax min (bitHeap.width - columnIndex)
          if (searchWidthMax >= rowAdder.widthMin) {
            var searchWidth = searchWidthMax
            while (searchWidth >= rowAdder.widthMin) {
              val searchedRowAdder = getCompressor(rowAdder.name.split('_').head, searchWidth)
              val exactScores      = getExactScores(searchedRowAdder, columnIndex, shouldPipeline = true)
              if (exactScores.reductionEfficiency >= (bestReductionEfficiency max 1.0)) {
                if (exactScores.reductionEfficiency == bestReductionEfficiency) {
                  if (exactScores.heightReduction >= bestHeightReduction) {
                    bestReductionEfficiency = exactScores.reductionEfficiency
                    bestCompressor          = searchedRowAdder
                    bestHeightReduction     = exactScores.heightReduction
                  }
                } else {
                  bestReductionEfficiency = exactScores.reductionEfficiency
                  bestCompressor          = searchedRowAdder
                  bestHeightReduction     = exactScores.heightReduction
                }
              }

              if ((exactScores.reductionEfficiency + 0.2) < 1.0) {
                searchWidth = 8 * floor((searchWidth - 1) / 8).toInt
              } else
                searchWidth -= 1
            }
          }
      }
      bestCompressor match {
        case gpc: Gpc           => CompressorStepSolution(gpc.name.split('_').head, -1, columnIndex, getExactScores(gpc, columnIndex, shouldPipeline = true))
        case rowAdder: RowAdder => CompressorStepSolution(rowAdder.name.split('_').head, rowAdder.width, columnIndex, getExactScores(rowAdder, columnIndex, shouldPipeline = true))
      }
    }

    val steps    = ArrayBuffer[CompressorStepSolution]()
    val heapOuts = ArrayBuffer[BitHeap[BigInt]]()
    while (nonEmpty) {
      val stepSolution = solveStep
      steps += stepSolution
      heapOuts += implStepSoft(stepSolution)
    }
    val heapNext: BitHeap[BigInt] = heapOuts.reduce(_ + _)
    asSoft.absorbHeapFrom(heapNext)
    CompressorStageSolution(steps, heightMax, pipelined = true)
  }
}

object TailOptimizedSlover extends BitHeapSolver {
  def apply(bitHeapGroup: BitHeapGroup[BigInt]): CompressorFullSolution = solveAll(bitHeapGroup)

  def apply(bitHeap: BitHeap[BigInt]): CompressorFullSolution = solveAll(bitHeap)

  override def solveStage(bitHeap: BitHeap[BigInt]): CompressorStageSolution = ???
}

object TailAndHeightOptimizedSlover extends BitHeapSolver {
  def apply(bitHeapGroup: BitHeapGroup[BigInt]): CompressorFullSolution = solveAll(bitHeapGroup)

  def apply(bitHeap: BitHeap[BigInt]): CompressorFullSolution = solveAll(bitHeap)

  override def solveStage(bitHeap: BitHeap[BigInt]): CompressorStageSolution = ???
}
