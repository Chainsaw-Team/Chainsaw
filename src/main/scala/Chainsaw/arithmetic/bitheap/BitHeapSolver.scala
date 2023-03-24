package Chainsaw.arithmetic.bitheap

import Chainsaw.arithmetic.{Compressor3to1, Compressor6to3}
import Chainsaw._
import Chainsaw.arithmetic._

import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.collection._

/** the base class of BitHeap solver, it define all the method which solver need
  * @note
  *   it has some method should implement by sub-class
  */
abstract class BitHeapSolver {

  /** this method is used to get this solver's name
    * @return
    *   this solver's name
    */
  def solverName = className(this)

  // for solver logger
  var solveStageIndex = 1

  // for strategy to get next time heap
  var nextTimeHeap: Option[BitHeap[BigInt]] = None
  var nextHeapCarry: mutable.Map[Int, Int]  = mutable.Map[Int, Int]()

  // for save row adder search time
  var searchThreshold = 0.2

  // infer pipeline related
  var inferPipelineMode = false

  var startInferPipeline = false

  var headFlipPipelineGap = 0

  var tailFlipPipelineGap = 1

  var pipelineState = true

  private var recordHeadGap = headFlipPipelineGap
  private var recordTailGap = tailFlipPipelineGap

  /** this method is used to auto refresh this solver's pipelineState
    * @param bitHeap
    *   the BitHeap which this refresh method will use
    * @return
    *   the new pipelineState
    */
  def refreshPipelineState(bitHeap: BitHeap[BigInt]): Boolean = {
    (bitHeap.reachLastStage, inferPipelineMode && startInferPipeline) match {
      case (true, true) =>
        if (recordTailGap == 1) {
          pipelineState = !pipelineState
          recordTailGap = tailFlipPipelineGap
        } else {
          recordTailGap -= 1
        }
      case (false, true) =>
        if (recordHeadGap == 1) {
          pipelineState = !pipelineState
          recordHeadGap = headFlipPipelineGap
        } else {
          recordHeadGap -= 1
        }
      case _ =>
    }
    pipelineState
  }

  /** this method is used to copy the solver configuration information
    * @param solver
    *   the solver which provides configuration information
    * @return
    *   this solver with new configuration information
    */
  def absorbMetaInfosFrom(solver: BitHeapSolver): BitHeapSolver = {
    searchThreshold     = solver.searchThreshold
    inferPipelineMode   = solver.inferPipelineMode
    startInferPipeline  = solver.startInferPipeline
    headFlipPipelineGap = solver.headFlipPipelineGap
    tailFlipPipelineGap = solver.tailFlipPipelineGap
    pipelineState       = solver.pipelineState
    solveStageIndex     = 1
    this
  }

  /* ----- core method which must be kept ----- */

  /** this method is used to solve a [[BitHeapGroup]] by existing compressor list
    * @param bitHeapGroup
    *   the [[BitHeapGroup]] will be solved
    * @return
    *   the [[CompressorFullSolution]] after solving
    */
  def solveAll(bitHeapGroup: BitHeapGroup[BigInt]): CompressorFullSolution = {
    bitHeapGroup.absorbConstant()

    val timeAndHeaps: Map[Int, BitHeap[BigInt]] =
      bitHeapGroup.bitHeaps.groupBy(_.time).map(pair => pair._1 -> pair._2.head)
    val stages = ArrayBuffer[CompressorStageSolution]()

    var currentTime = timeAndHeaps.keys.min
    var currentHeap = timeAndHeaps(currentTime)

    while (currentTime < timeAndHeaps.keys.max) {
      nextTimeHeap = timeAndHeaps.get(currentTime + 1)
      stages += solveStage(currentHeap)
      if (stages.last.pipelined) {
        timeAndHeaps.get(currentTime + 1) match {
          case Some(heapNext) =>
            heapNext.absorbHeapFrom(currentHeap)
            currentHeap = heapNext
          case None =>
        }
        currentTime += 1
      }
    }
    startInferPipeline = true
    while (currentHeap.heightMax > 3) {
      stages += solveStage(currentHeap)
      if (verbose >= 1)
        logger.info(s"${if (stages.last.pipelined) "stage pipelined"
        else "stage not pipeline"}")
    }
    solveStageIndex = 1
    CompressorFullSolution(stages)
  }

  /** this method is used to solve a [[BitHeap]] one stage by existing compressor list
    * @param bitHeap
    *   the [[BitHeap]] will be solved
    * @return
    *   the [[CompressorStageSolution]] after solving
    */
  def solveStage(bitHeap: BitHeap[BigInt]): CompressorStageSolution = {
    import bitHeap._
//    logger.info(s"soft solve heap\n$bitHeap")
    nextHeapCarry.clear()
    val initialBitCount    = bitsCount
    val initialHeightMax   = heightMax
    var totalCost          = 0.0
    var stagePipelineState = refreshPipelineState(bitHeap)

    val steps    = ArrayBuffer[CompressorStepSolution]()
    val heapOuts = ArrayBuffer[BitHeap[BigInt]]()
    while (nonEmpty) {
      val stepSolution = solveStep(bitHeap)
      steps += stepSolution
      totalCost += stepSolution.compressorScores.cost
      heapOuts += implStepSoft(stepSolution)
    }
    require(isEmpty, s"Incomplete solve in stage $solveStageIndex")

    val heapNext: BitHeap[BigInt] = heapOuts.reduce(_ + _)
    asSoft.absorbHeapFrom(heapNext)
    if (verbose >= 1)
      logger.info(
        s"----------------Solve stage $solveStageIndex by $this----------------"
      )
    solveStageIndex += 1
    if (heightMax <= 3 || reachLastStage) stagePipelineState = true
    if (stagePipelineState) dSoft()
    CompressorStageSolution(
      steps,
      initialHeightMax,
      heightMax,
      pipelined = stagePipelineState,
      solveStageIndex - 1
    )
  }

  /** this method is used to solve a [[BitHeap]] one step by existing compressor list
    * @param bitHeap
    *   the [[BitHeap]] will be solved
    * @return
    *   the [[CompressorStepSolution]] after solving
    */
  def solveStep(bitHeap: BitHeap[BigInt]): CompressorStepSolution

  /** This method is used to simplify the way the solver is called
    * @param bitHeapGroup
    *   the [[BitHeapGroup]] will be solved
    * @return
    *   the [[CompressorFullSolution]] after solving
    */
  def apply(bitHeapGroup: BitHeapGroup[BigInt]): CompressorFullSolution =
    solveAll(bitHeapGroup)

  /** This method is used to simplify the way the solver is called when solve a BitHeap
    * @param bitHeap
    *   the [[BitHeap]] will be solved
    * @return
    *   the [[CompressorFullSolution]] after solving
    */
  def apply(bitHeap: BitHeap[BigInt]): CompressorFullSolution = solveAll(
    BitHeapGroup(ArrayBuffer(bitHeap))
  )

  type TestCase = Seq[ArithInfo]

  /** override the toString method, it is used to get solver's name
    * @return
    *   the name of solver
    */
  override def toString: String = solverName

}

/** the naive solver which use fixed compressor at different compress stage
  * @note
  *   In the head stage, use Compressor3to1. In the tail stage, use Compressor6to3
  */
object NaiveSolver extends BitHeapSolver {

  override def solveStep(bitHeap: BitHeap[BigInt]): CompressorStepSolution = {

    import bitHeap._

    def isLastStage = heightMax <= 6

    if (!isLastStage) {
      val maxHeightColumnIndex = heap.indexWhere(_.nonEmpty)
      val endIdx               = heap.lastIndexWhere(_.nonEmpty)
      val width                = ((endIdx - maxHeightColumnIndex + 1) min cpaWidthMax) max 8
      CompressorStepSolution(
        compressorName = "Compressor3to1",
        width,
        maxHeightColumnIndex,
        getExactScores(
          Compressor3to1(width),
          maxHeightColumnIndex,
          shouldPipeline = true
        )
      )
    } else {
      val maxHeightColumnIndex = heap.indexWhere(_.nonEmpty)
      CompressorStepSolution(
        compressorName = "Compressor6to3",
        width,
        maxHeightColumnIndex,
        getExactScores(
          Compressor6to3(),
          maxHeightColumnIndex,
          shouldPipeline = true
        )
      )
    }
  }
}

/** the greed solver which search the compressor with best reduction efficiency in every step solve
  */
object GreedSolver extends BitHeapSolver {

  override def solveStep(bitHeap: BitHeap[BigInt]): CompressorStepSolution = {

    import bitHeap._

    val maxHeightColumnIndex    = heights.indexWhere(_ == heightMax)
    var bestCompressor          = getCompressor("Compressor1to1", 1)
    var bestReductionEfficiency = 0.0
    var bestHeightReduction     = bestCompressor.heightReduction

    val candidates = candidateCompressors
      .sortBy(compressor => compressor.reductionEfficiency)
      .reverse

    candidates.foreach { compressor =>
      // get covered complementHeap
      val bitCoveredComplementHeap =
        compressor.inputFormat
          .zip(complementHeap.drop(maxHeightColumnIndex))
          .map { case (h, cHeap) =>
            cHeap.take(h)
          }
      compressor match {
        case gpc: Gpc =>
          val searchedGpc =
            getCompressor(gpc.name, -1, bitCoveredComplementHeap)
          val exactScores =
            getExactScores(
              searchedGpc,
              maxHeightColumnIndex,
              pipelineState
            )
          if (exactScores.reductionEfficiency >= bestReductionEfficiency) {
            if (exactScores.reductionEfficiency == bestReductionEfficiency) {
              if (exactScores.heightReduction >= bestHeightReduction) {
                bestReductionEfficiency = exactScores.reductionEfficiency
                bestCompressor          = searchedGpc
                bestHeightReduction     = exactScores.heightReduction
              }
            } else {
              bestReductionEfficiency = exactScores.reductionEfficiency
              bestCompressor          = searchedGpc
              bestHeightReduction     = exactScores.heightReduction
            }
          }
        case rowAdder: RowAdder =>
          val searchWidthMax =
            rowAdder.widthMax min (bitHeap.width - maxHeightColumnIndex)
          if (searchWidthMax >= rowAdder.widthMin) {
            var searchWidth = searchWidthMax
            while (searchWidth >= rowAdder.widthMin) {
              val searchedRowAdder =
                getCompressor(
                  rowAdder.name,
                  searchWidth,
                  bitCoveredComplementHeap
                )
              val exactScores = getExactScores(
                searchedRowAdder,
                maxHeightColumnIndex,
                pipelineState
              )
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

              if ((exactScores.reductionEfficiency + searchThreshold) < 1.0) {
                searchWidth = 8 * floor((searchWidth - 1) / 8).toInt
              } else
                searchWidth -= 1
            }
          }
      }
    }
    bestCompressor match {
      case gpc: Gpc =>
        CompressorStepSolution(
          gpc.name,
          -1,
          maxHeightColumnIndex,
          getExactScores(gpc, maxHeightColumnIndex, pipelineState)
        )
      case rowAdder: RowAdder =>
        CompressorStepSolution(
          rowAdder.name,
          rowAdder.width,
          maxHeightColumnIndex,
          getExactScores(
            rowAdder,
            maxHeightColumnIndex,
            pipelineState
          )
        )
    }

  }
}

/** the base class which contain two strategy for compressor search(headStrategy, tailStrategy)
  * @param headBound
  *   the efficiency bound in headStrategy search, it means that if the compressor's reduction efficiency is less than
  *   this bound, it will be skipped
  * @param tailBound
  *   the efficiency bound in tailStrategy search, it means that if the compressor's reduction efficiency is less than
  *   this bound, it will be skipped
  */
abstract class ConditionalStrategySolver(
    val headBound: Double,
    val tailBound: Double
) extends BitHeapSolver {

  private var useHeadStrategy = true
  inferPipelineMode = true

  /** this method is used to solve given [[BitHeap]] using headStrategy
    * @param bitHeap
    *   the [[BitHeap]] will be solved
    * @return
    *   the [[CompressorStepSolution]] after solving
    */
  def headStrategy(bitHeap: BitHeap[BigInt]): CompressorStepSolution

  /** this method is used to solve given [[BitHeap]] using tailStrategy
    * @param bitHeap
    *   the [[BitHeap]] will be solved
    * @return
    *   the [[CompressorStepSolution]] after solving
    */
  def tailStrategy(bitHeap: BitHeap[BigInt]): CompressorStepSolution

  override def solveStep(bitHeap: BitHeap[BigInt]): CompressorStepSolution =
    if (useHeadStrategy) headStrategy(bitHeap) else tailStrategy(bitHeap)

  override def solveStage(bitHeap: BitHeap[BigInt]): CompressorStageSolution = {
    useHeadStrategy = !bitHeap.reachLastStage
    if (verbose >= 1)
      logger.info(
        s"use ${if (useHeadStrategy) "HeadStrategy" else "TailStrategy"} in Step solve"
      )
    val stageSolution = super.solveStage(bitHeap)
    stageSolution
  }
}

/** the strategy separation solver which use different strategy in head stage and tail stage
  * @note
  *   In head stage, use reduction efficiency first strategy. In tail stage, use height reduction first strategy and
  *   only search gpc
  */
object StrategySeparationSolver extends ConditionalStrategySolver(headBound = 1.0, tailBound = 0.2) {

  val greedSolver = GreedSolver

  override def headStrategy(
      bitHeap: BitHeap[BigInt]
  ): CompressorStepSolution = {
    greedSolver.absorbMetaInfosFrom(this)
    greedSolver.solveStep(bitHeap)
  }

  override def tailStrategy(
      bitHeap: BitHeap[BigInt]
  ): CompressorStepSolution = {
    import bitHeap._

    val maxHeightColumnIndex    = heights.indexWhere(_ == heightMax)
    var bestCompressor          = getCompressor("Compressor1to1", 1)
    var bestReductionEfficiency = 0.0
    var bestHeightReduction     = bestCompressor.heightReduction

    val candidates = candidateCompressors
      .sortBy(compressor => compressor.reductionEfficiency)
      .reverse

    candidates.foreach { compressor =>
      // get covered complementHeap
      val bitCoveredComplementHeap =
        compressor.inputFormat
          .zip(complementHeap.drop(maxHeightColumnIndex))
          .map { case (h, cHeap) =>
            cHeap.take(h)
          }
      compressor match {
        case gpc: Gpc =>
          val searchedGpc =
            getCompressor(gpc.name, -1, bitCoveredComplementHeap)
          val exactScores =
            getExactScores(
              searchedGpc,
              maxHeightColumnIndex,
              shouldPipeline = pipelineState
            )
          if (exactScores.heightReduction >= bestHeightReduction) {
            if (exactScores.heightReduction == bestHeightReduction) {
              if (exactScores.reductionEfficiency >= bestReductionEfficiency) {
                bestReductionEfficiency = exactScores.reductionEfficiency
                bestCompressor          = searchedGpc
                bestHeightReduction     = exactScores.heightReduction
              }
            } else {
              bestReductionEfficiency = exactScores.reductionEfficiency
              bestCompressor          = searchedGpc
              bestHeightReduction     = exactScores.heightReduction
            }
          }
        case rowAdder: RowAdder =>
          val searchWidthMax =
            rowAdder.widthMax min (bitHeap.width - maxHeightColumnIndex)
          if (searchWidthMax >= rowAdder.widthMin) {
            var searchWidth = searchWidthMax
            while (searchWidth >= rowAdder.widthMin) {
              val searchedRowAdder =
                getCompressor(
                  rowAdder.name,
                  searchWidth,
                  bitCoveredComplementHeap
                )
              val exactScores = getExactScores(
                searchedRowAdder,
                maxHeightColumnIndex,
                shouldPipeline = pipelineState
              )
              if (exactScores.heightReduction >= bestHeightReduction) {
                if (exactScores.heightReduction == bestHeightReduction) {
                  if (exactScores.reductionEfficiency >= (bestReductionEfficiency max 1.0)) {
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

              if ((exactScores.reductionEfficiency + searchThreshold) < 1.0) {
                searchWidth = 8 * floor((searchWidth - 1) / 8).toInt
              } else
                searchWidth -= 1
            }
          }
      }
    }
    bestCompressor match {
      case gpc: Gpc =>
        CompressorStepSolution(
          gpc.name,
          -1,
          maxHeightColumnIndex,
          getExactScores(gpc, maxHeightColumnIndex, pipelineState)
        )
      case rowAdder: RowAdder =>
        CompressorStepSolution(
          rowAdder.name,
          rowAdder.width,
          maxHeightColumnIndex,
          getExactScores(
            rowAdder,
            maxHeightColumnIndex,
            pipelineState
          )
        )
    }
  }
}

/** the optimization of strategy separation solver
  * @note
  *   In head stage, use reduction efficiency first strategy and height control. In tail stage, use height reduction
  *   first strategy and height control.
  * @note
  *   In stage solve, it will consider all carry in from right column and the [[BitHeap]] which arrive next time
  */
object StrategySeparationOptimizedSolver extends ConditionalStrategySolver(headBound = 1.0, tailBound = 0.2) {

  /** this method is used to decide whether can skip search, it means that use [[Compressor1to1]] is best in this solve
    * @param heap
    *   the [[BitHeap]] will be solved
    * @return
    *   the result which indicate whether can skip, true means that it can skip
    */
  def isNeedSkipSearch(heap: BitHeap[BigInt]): Boolean = {
    import heap._

    val maxHeightColumnIndex                                          = heights.indexWhere(_ == heightMax)
    var nextHeapHeightInCurrentPosition, nextHeapHeightInNextPosition = 0
    nextTimeHeap match {
      case Some(nextHeap) =>
        val positionInNextHeap =
          weightLow + maxHeightColumnIndex - nextHeap.weightLow
        if (positionInNextHeap == -1)
          nextHeapHeightInNextPosition = nextHeap.heights.head
        if (positionInNextHeap >= 0) {
          if (positionInNextHeap <= nextHeap.heights.length - 2) {
            nextHeapHeightInCurrentPosition = nextHeap.heights(positionInNextHeap)
            nextHeapHeightInNextPosition    = nextHeap.heights(positionInNextHeap + 1)
          }
          if (positionInNextHeap == nextHeap.heights.length - 1) {
            nextHeapHeightInCurrentPosition = nextHeap.heights(positionInNextHeap)
          }
        }
      case None =>
    }

    var skipCompressorSearch = heights(maxHeightColumnIndex) == 1

    /** this method is used to indicate whether current column need to keep bits, don't need compress
      * @return
      *   the result which indicate whether current column need to keep bits, don't need compress
      */
    def currentColumnNeedKeep = heights(maxHeightColumnIndex) +
      nextHeapCarry.getOrElse(maxHeightColumnIndex, 0) +
      nextHeapHeightInCurrentPosition == 3

    /** this method is used to indicate whether current column need to accept bits
      * @return
      *   the result which indicate whether current column need to accept bits
      */
    def nextColumnNeedCarry = heights(maxHeightColumnIndex + 1) +
      nextHeapCarry.getOrElse(maxHeightColumnIndex + 1, 0) +
      nextHeapHeightInNextPosition == 2
    val needConsiderSkipSearch =
      heights(maxHeightColumnIndex) == 2 && maxHeightColumnIndex <= width - 2
    val needConsiderLastColumn =
      heights(maxHeightColumnIndex) == 2 && maxHeightColumnIndex == width - 1
    if (needConsiderSkipSearch) {
      if (pipelineState)
        skipCompressorSearch    = currentColumnNeedKeep || (!currentColumnNeedKeep && !nextColumnNeedCarry)
      else skipCompressorSearch = currentColumnNeedKeep
    } else if (needConsiderLastColumn) {
      if (pipelineState)
        skipCompressorSearch = nextHeapCarry.getOrElse(maxHeightColumnIndex, 0) +
          nextHeapHeightInCurrentPosition == 0
      else
        skipCompressorSearch = nextHeapCarry.getOrElse(maxHeightColumnIndex, 0) +
          nextHeapHeightInCurrentPosition <= 1
    }
    skipCompressorSearch
  }

  /** this method is used to compare two local optimal compressor and give a result to indicate which compressor is best
    * @param heap
    *   the [[BitHeap]] will be solved
    * @param bestRowAdder
    *   one of the two local optimal compressor
    * @param candidateRowAdder
    *   one of the two local optimal compressor
    * @param startColumnIndex
    *   the start index which bits be covered
    * @return
    *   the compare result, if true, means that the [[bestRowAdder]] better than the [[candidateRowAdder]]
    */
  def compareRowAdder(
      heap: BitHeap[BigInt],
      bestRowAdder: RowAdder,
      candidateRowAdder: RowAdder,
      startColumnIndex: Int
  ): Boolean = {
    import heap._
    val rowAdders = ArrayBuffer(bestRowAdder, candidateRowAdder)
    val widths    = rowAdders.map(_.width)
    val candidateHeaps = Seq.fill(2)(
      BitHeap.fromHeights(
        heights.drop(startColumnIndex),
        weightLow + startColumnIndex,
        time = 0
      )
    )

    val firstStageReductionBits  = ArrayBuffer.fill(2)(0)
    val firstStageCost           = ArrayBuffer(0.0, 0.0)
    val secondStageReductionBits = ArrayBuffer.fill(2)(ArrayBuffer(0, 0))
    val secondStageCost          = ArrayBuffer.fill(2)(ArrayBuffer(0.0, 0.0))

    rowAdders.zipWithIndex
      .foreach { case (compressor, i) =>
        firstStageCost(i) = compressor.clbCost
        firstStageReductionBits(i) = compressor.inputFormat
          .zip(candidateHeaps(i).heap)
          .map { case (number, column) =>
            val exactNumber = column.length min number
            val slice       = column.take(exactNumber)
            column --= slice
            slice
          }
          .map(_.length)
          .sum - compressor.outputBitsCount
      }

    candidateHeaps
      .zip(Seq.fill(2)(rowAdders).zipWithIndex)
      .foreach { case (bitHeap, (candidateComp, i)) =>
        if (bitHeap.isEmpty) {
          secondStageCost(i)          = ArrayBuffer(0.0, 0.0)
          secondStageReductionBits(i) = ArrayBuffer(0, 0)
        } else {
          val startColumnIndex    = bitHeap.heights.indexWhere(_ == bitHeap.heightMax)
          val stageBestEfficiency = ArrayBuffer.fill(2)(ArrayBuffer(0.0, 0.0))
          val stageBestCompressor = ArrayBuffer.fill(2)(rowAdders)
          val stageBestWidth      = ArrayBuffer.fill(2)(widths)

          candidateComp.zipWithIndex.foreach { case (compressor, j) =>
            var searchWidth =
              (heights.length - startColumnIndex) min compressor.widthMax
            while (searchWidth >= compressor.widthMin) {
              val exactReductionEfficiency =
                bitHeap
                  .getExactScores(
                    compressor,
                    startColumnIndex,
                    pipelineState
                  )
                  .reductionEfficiency
              if (
                exactReductionEfficiency >= (stageBestEfficiency(i)(
                  j
                ) max headBound)
              ) {
                stageBestEfficiency(i)(j) = exactReductionEfficiency
                stageBestCompressor(i)(j) = compressor
                stageBestWidth(i)(j)      = searchWidth
              }
              if ((exactReductionEfficiency + searchThreshold) < headBound)
                searchWidth = 8 * floor((searchWidth - 1) / 8).toInt
              else searchWidth -= 1
            }
          }

          stageBestEfficiency(i).zipWithIndex
            .zip(stageBestCompressor(i).zip(stageBestWidth(i)))
            .foreach { case ((re, j), (comp, width)) =>
              if (re >= headBound) {
                val scores = bitHeap.getExactScores(comp, startColumnIndex, pipelineState)
                secondStageCost(i)(j)          = scores.cost
                secondStageReductionBits(i)(j) = scores.bitReduction
              } else {
                secondStageCost(i)(j) = bitHeap.heap
                  .take(comp.width)
                  .map { col =>
                    col.length match {
                      case i if i <= 2          => i * 0.5
                      case i if i >= 3 && i < 6 => 1.0
                      case i if i >= 6          => 3.0
                    }
                  }
                  .sum
                secondStageReductionBits(i)(j) = bitHeap.heap
                  .take(comp.width)
                  .map { col =>
                    col.length match {
                      case i if i <= 2          => 0
                      case i if i >= 3 && i < 6 => 1
                      case i if i >= 6          => 3
                    }
                  }
                  .sum
              }
            }
        }

      }

    val finalReductionBits =
      firstStageReductionBits.zip(secondStageReductionBits).map { case (first, second) =>
        second.map(_ + first)
      }
    val finalCost = firstStageCost.zip(secondStageCost).map { case (first, second) =>
      second.map(_ + first)
    }
    val finalEfficiency = finalReductionBits.zip(finalCost).map { case (reductions, costs) =>
      reductions.zip(costs).map { case (reduction, cost) => reduction / cost }.max
    }
    var compareResult = true
    if (
      finalEfficiency.head < finalEfficiency.last || (finalEfficiency.head == finalEfficiency.last && bestRowAdder.inputFormat.sum < candidateRowAdder.inputFormat.sum)
    ) compareResult = false
    compareResult
  }

  override def headStrategy(
      bitHeap: BitHeap[BigInt]
  ): CompressorStepSolution = {
    import bitHeap._

    // find the first(lowest weight) column with maximum height
    val maxHeightColumnIndex    = heights.indexWhere(_ == heightMax)
    var bestCompressor          = getCompressor("Compressor1to1", 1)
    var bestReductionEfficiency = 0.0
    var bestHeightReduction     = bestCompressor.heightReduction

    candidateCompressors.foreach(_.setPipelineState(pipelineState))
    val candidates = candidateCompressors.tail
      .sortBy(compressor => compressor.reductionEfficiency)
      .reverse

    if (!isNeedSkipSearch(bitHeap)) {
      var exactScores = ScoreIndicator(0, 0, 0, 0, 0)
      def betterEfficiency =
        exactScores.reductionEfficiency > (bestReductionEfficiency max headBound)
      def sameEfficiency =
        exactScores.reductionEfficiency == (bestReductionEfficiency max headBound)
      def betterHeightReduction =
        exactScores.heightReduction >= bestHeightReduction

      candidates.foreach { compressor =>
        // get covered complementHeap
        val bitCoveredComplementHeap =
          compressor.inputFormat
            .zip(complementHeap.drop(maxHeightColumnIndex))
            .map { case (h, cHeap) =>
              cHeap.take(h)
            }
        compressor match {
          case gpc: Gpc =>
            val searchedGpc =
              getCompressor(gpc.name, -1, bitCoveredComplementHeap)
            exactScores = getExactScores(
              searchedGpc,
              maxHeightColumnIndex,
              pipelineState
            )

            if (betterEfficiency) {
              bestReductionEfficiency = exactScores.reductionEfficiency
              bestCompressor          = searchedGpc
              bestHeightReduction     = exactScores.heightReduction
            } else if (sameEfficiency && betterHeightReduction) {
              bestReductionEfficiency = exactScores.reductionEfficiency
              bestCompressor          = searchedGpc
              bestHeightReduction     = exactScores.heightReduction
            }
          case rowAdder: RowAdder =>
            val searchWidthMax =
              rowAdder.widthMax min (bitHeap.width - maxHeightColumnIndex)
            if (searchWidthMax >= rowAdder.widthMin) {
              var searchWidth = searchWidthMax
              while (searchWidth >= rowAdder.widthMin) {
                val searchedRowAdder =
                  getCompressor(
                    rowAdder.name,
                    searchWidth,
                    bitCoveredComplementHeap
                  ).asInstanceOf[RowAdder]
                exactScores = getExactScores(
                  searchedRowAdder,
                  maxHeightColumnIndex,
                  pipelineState
                )

                bestCompressor match {
                  case adder: RowAdder =>
                    if (
                      compareRowAdder(
                        bitHeap,
                        searchedRowAdder,
                        adder,
                        maxHeightColumnIndex
                      )
                    ) {
                      bestReductionEfficiency = exactScores.reductionEfficiency
                      bestCompressor          = searchedRowAdder
                      bestHeightReduction     = exactScores.heightReduction
                    }
                  case _ =>
                    if (betterEfficiency) {
                      bestReductionEfficiency = exactScores.reductionEfficiency
                      bestCompressor          = searchedRowAdder
                      bestHeightReduction     = exactScores.heightReduction
                    } else if (sameEfficiency && betterHeightReduction) {
                      bestReductionEfficiency = exactScores.reductionEfficiency
                      bestCompressor          = searchedRowAdder
                      bestHeightReduction     = exactScores.heightReduction
                    }
                }

                if ((exactScores.reductionEfficiency + searchThreshold) < 1.0) {
                  searchWidth = 8 * floor((searchWidth - 1) / 8).toInt
                } else
                  searchWidth -= 1
              }
            }
        }
      }

      if (bestCompressor.outputFormat.length > 1) {
        bestCompressor.outputFormat.zipWithIndex.foreach { case (h, i) =>
          val oldRecord = nextHeapCarry.get(maxHeightColumnIndex + i)
          oldRecord match {
            case Some(old) =>
              nextHeapCarry.remove(maxHeightColumnIndex + i)
              nextHeapCarry.put(maxHeightColumnIndex + i, old + h)
            case None =>
              nextHeapCarry.put(maxHeightColumnIndex + i, h)
          }
        }
      }
    }
    bestCompressor match {
      case gpc: Gpc =>
        CompressorStepSolution(
          gpc.name,
          -1,
          maxHeightColumnIndex,
          getExactScores(gpc, maxHeightColumnIndex, pipelineState)
        )
      case rowAdder: RowAdder =>
        CompressorStepSolution(
          rowAdder.name,
          rowAdder.width,
          maxHeightColumnIndex,
          getExactScores(
            rowAdder,
            maxHeightColumnIndex,
            pipelineState
          )
        )
    }
  }

  override def tailStrategy(
      bitHeap: BitHeap[BigInt]
  ): CompressorStepSolution = {
    import bitHeap._

    val maxHeightColumnIndex    = heights.indexWhere(_ == heightMax)
    var bestCompressor          = getCompressor("Compressor1to1", 1)
    var bestReductionEfficiency = 0.0
    var bestHeightReduction     = bestCompressor.heightReduction

    val gpcs = candidateCompressors
      .filter(_.isInstanceOf[Gpc])
      .sortBy(compressor => compressor.reductionEfficiency)
      .reverse

    val rowAdderEfficiencyTable = candidateCompressors
      .filter(_.isInstanceOf[RowAdder])
      .flatMap { rowAdder =>
        val (widthMax, widthMin) = (
          rowAdder
            .asInstanceOf[RowAdder]
            .widthMax,
          rowAdder
            .asInstanceOf[RowAdder]
            .widthMin
        )
        // get covered complementHeap
        val bitCoveredComplementHeap =
          rowAdder.inputFormat
            .zip(complementHeap.drop(maxHeightColumnIndex))
            .map { case (h, cHeap) =>
              cHeap.take(h)
            }
        Range
          .inclusive(
            (width - maxHeightColumnIndex) min widthMax,
            1,
            -1
          )
          .filter(_ >= widthMin)
          .map { w =>
            getCompressor(rowAdder.name, w, bitCoveredComplementHeap)
              .asInstanceOf[RowAdder] ->
              getExactScores(
                rowAdder,
                maxHeightColumnIndex,
                pipelineState
              ).reductionEfficiency
          }
      }
    val maxEfficiencyRowAdder = rowAdderEfficiencyTable
      .filter { case (_, efficiency) =>
        efficiency == rowAdderEfficiencyTable.maxBy(_._2)._2
      }
      .maxBy(_._1.heightReduction)

//    val needGpcSearch = if (maxEfficiencyRowAdder._2 > 1.0) {
//      val rowAdderWidth = maxEfficiencyRowAdder._1.inputFormat.length
//      if (!pipelineState) {
//        if (
//          rowAdderWidth + maxHeightColumnIndex != width && heights(
//            rowAdderWidth + maxHeightColumnIndex
//          ) + nextHeapCarry.getOrElse(
//            rowAdderWidth + maxHeightColumnIndex,
//            0
//          ) > 1
//        ) true
//        else false
//      } else {
//        if (rowAdderWidth + maxHeightColumnIndex != width) true
//        else false
//      }
//    } else true

    val needGpcSearch = if (maxEfficiencyRowAdder._2 > 1.0) {
      val rowAdderWidth = maxEfficiencyRowAdder._1.inputFormat.length
      if (
        rowAdderWidth + maxHeightColumnIndex != width && nextHeapCarry.getOrElse(
          rowAdderWidth + maxHeightColumnIndex,
          0
        ) > 1
      ) true
      else false
    } else true

    if (!needGpcSearch) {
      bestCompressor          = maxEfficiencyRowAdder._1
      bestReductionEfficiency = maxEfficiencyRowAdder._2
    }

    if (!isNeedSkipSearch(bitHeap) && needGpcSearch) {
      gpcs.foreach { gpc =>
        val exactScores =
          getExactScores(
            gpc,
            maxHeightColumnIndex,
            shouldPipeline = pipelineState
          )
        if (exactScores.heightReduction >= bestHeightReduction) {
          if (exactScores.heightReduction == bestHeightReduction) {
            if (exactScores.reductionEfficiency >= bestReductionEfficiency) {
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
      }
    }

    if (bestCompressor.outputFormat.length > 1) {
      bestCompressor.outputFormat.zipWithIndex.foreach { case (h, i) =>
        val oldRecord = nextHeapCarry.get(maxHeightColumnIndex + i)
        oldRecord match {
          case Some(old) =>
            nextHeapCarry.remove(maxHeightColumnIndex + i)
            nextHeapCarry.put(maxHeightColumnIndex + i, old + h)
          case None =>
            nextHeapCarry.put(maxHeightColumnIndex + i, h)
        }
      }
    }

    bestCompressor match {
      case gpc: Gpc =>
        CompressorStepSolution(
          gpc.name,
          -1,
          maxHeightColumnIndex,
          getExactScores(gpc, maxHeightColumnIndex, pipelineState)
        )
      case rowAdder: RowAdder =>
        CompressorStepSolution(
          rowAdder.name,
          rowAdder.width,
          maxHeightColumnIndex,
          getExactScores(
            rowAdder,
            maxHeightColumnIndex,
            pipelineState
          )
        )
    }
  }

}

/** the ternaryTree solver which use [[Compressor3to1]] to complete the bits compress
  */
object TernaryTreeSolver extends BitHeapSolver {

  override def solveStep(bitHeap: BitHeap[BigInt]): CompressorStepSolution = {
    import bitHeap._
    val maxHeightColumnIndex = heap.indexWhere(_.nonEmpty)
    val endIdx               = heap.lastIndexWhere(_.nonEmpty)
    val width                = ((endIdx - maxHeightColumnIndex + 1) min cpaWidthMax) max 8
    CompressorStepSolution(
      compressorName = "Compressor3to1",
      width,
      maxHeightColumnIndex,
      getExactScores(
        Compressor3to1(width),
        maxHeightColumnIndex,
        pipelineState
      )
    )
  }
}

/** the gpc solver which only use [[Gpc]] to complete the bits compress
  */
object GpcSolver extends BitHeapSolver {

  override def solveStep(bitHeap: BitHeap[BigInt]): CompressorStepSolution = {
    import bitHeap._
    val maxHeightColumnIndex = heap.indexWhere(_.nonEmpty)
    val bestCompressor = Gpcs().maxBy(gpc => getExactScores(gpc, maxHeightColumnIndex, pipelineState).heightReduction)
    CompressorStepSolution(
      compressorName = bestCompressor.name.firstBeforeChar('_'),
      width,
      maxHeightColumnIndex,
      getExactScores(
        bestCompressor,
        maxHeightColumnIndex,
        pipelineState
      )
    )
  }
}
