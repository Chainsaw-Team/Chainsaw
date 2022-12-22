package Chainsaw.arithmetic.bitheap

import Chainsaw.arithmetic.{Compressor3to1, Compressor6to3}
import Chainsaw.{logger, verbose}
import Chainsaw.arithmetic._

import scala.collection.mutable.ArrayBuffer

abstract class BitHeapSolver {

  /** core method which must be kept
   */
  def solveAll(bitHeapGroup: BitHeapGroup[BigInt]): CompressorFullSolution = {

    val timeAndHeaps: Map[Int, BitHeap[BigInt]] = bitHeapGroup.bitHeaps.groupBy(_.time).map(pair => pair._1 -> pair._2.head)
    val stages = ArrayBuffer[CompressorStageSolution]()

    var currentTime = timeAndHeaps.keys.min
    var currentHeap = timeAndHeaps(currentTime)

    while (currentTime < timeAndHeaps.keys.max) {
      stages += solveStage(currentHeap)
      timeAndHeaps.get(currentTime + 1) match {
        case Some(heapNext) =>
          heapNext.absorbHeapFrom(currentHeap.dSoft())
          currentHeap = heapNext
        case None =>
          currentHeap = currentHeap.dSoft()
      }
      currentTime += 1
    }

    val restSolution = solveAll(currentHeap)
    CompressorFullSolution(stages ++ restSolution.stageSolutions)
  }

  def solveAll(bitHeap: BitHeap[BigInt]): CompressorFullSolution = {
    val stages = ArrayBuffer[CompressorStageSolution]()
    while (bitHeap.heightMax > 3) stages += solveStage(bitHeap)
    CompressorFullSolution(stages)
  }

  def solveStage(bitHeap: BitHeap[BigInt]): CompressorStageSolution

  type TestCase = Seq[ArithInfo]

}

object NaiveSolver extends BitHeapSolver {

  override def solveStage(bitHeap: BitHeap[BigInt]): CompressorStageSolution = {

    import bitHeap._

    def isLastStage = heightMax <= 6

    def solveStep: CompressorStepSolution = {
      if (!isLastStage) {
        val columnIdx = heap.indexWhere(_.nonEmpty)
        val endIdx = heap.lastIndexWhere(_.nonEmpty)
        val width = ((endIdx - columnIdx + 1) min cpaWidthMax) max 8
        CompressorStepSolution(compressorName = "Compressor3to1", width, columnIdx, getExactScores(Compressor3to1(width), columnIdx, shouldPipeline = true))
      }
      else {
        val columnIdx = heap.indexWhere(_.nonEmpty)
        CompressorStepSolution(compressorName = "Compressor6to3", width, columnIdx, getExactScores(Compressor6to3, columnIdx, shouldPipeline = true))
      }
    }

    val steps = ArrayBuffer[CompressorStepSolution]()
    val heapOuts = ArrayBuffer[BitHeap[BigInt]]()
    while (nonEmpty) {
      val stepSolution = solveStep
      steps += stepSolution
      heapOuts += implStepSoft(stepSolution)
    }
    val heapNext: BitHeap[BigInt] = heapOuts.reduce(_ + _)
    asSoft.absorbHeapFrom(heapNext)
    if (verbose >= 1) logger.info(s"----------------\n$this----------------\n")
    CompressorStageSolution(steps, pipelined = true)
  }
}

