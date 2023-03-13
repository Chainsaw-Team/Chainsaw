package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic._
import Chainsaw.xilinx.VivadoUtil
import spinal.core._
import java.io.File
import scala.language.postfixOps

/** enhanced multi-operand adder implemented by compressors
  */
case class BitHeapCompressor(
    arithInfos: Seq[ArithInfo],
    solver: BitHeapSolver = null
) extends UnsignedMerge {

  override def name =
    s"BitHeapCompressor_${hashName(arithInfos)}_${if (solver != null) className(solver)
    else "inferred"}"

  val bitHeapGroup = BitHeapGroup.fromInfos(arithInfos)
  val shape1       = bitHeapGroup.bitHeaps.map(_.heights)
//  logger.info(s"--------initial softType bitHeap[${hashName(arithInfos)}]--------\n$bitHeapGroup")
  override val positiveLength = bitHeapGroup.positiveLength
  val solutionFile            = new File(compressorSolutionOutputDir, s"$name")
  val solution = if (solutionFile.exists()) {
    logger.info(s"take existing solution from $solutionFile")
    CompressorFullSolution.load(solutionFile)
  } else {
    val solution =
      if (solver == null) searchBestSolver(bitHeapGroup, detailReport = true)
      else solver.solveAll(bitHeapGroup)
    solution.save(solutionFile)
    solution
  }

  // when the output is too wide for a single 3:1 compressor, we do 3:2 compression first
  val doFinal3to2 =
    solution.outHeight == 3 && bitHeapGroup.positiveLength > cpaWidthMax
  if (doFinal3to2)
    logger.info(
      s"add a final 3:2 compression stage as the output width ${bitHeapGroup.positiveLength} > $cpaWidthMax"
    )
  val finalHeight = if (doFinal3to2) 2 else solution.outHeight

  override def outputTypes: Seq[NumericType] =
    Seq.fill(finalHeight)(NumericType.U(positiveLength))

  override def outputArithInfos =
    Seq.fill(finalHeight)(ArithInfo(positiveLength, weightLow))

  override def vivadoUtilEstimation = solution.vivadoUtilEstimation +
    VivadoUtil(lut = doFinal3to2.toInt * positiveLength)

  override def fmaxEstimation = 600 MHz

  override def latency() = solution.latency + doFinal3to2.toInt

  override def implH = new ChainsawOperatorModule(this) {
    val operands = dataIn.map(_.asUInt())
    val weightedUInts = operands.zip(arithInfos).map { case (int, info) =>
      WeightedUInt(int, info)
    }
    val bitHeapGroup = BitHeapGroup.fromUInts(weightedUInts)
    val shape2       = bitHeapGroup.bitHeaps.map(_.heights)
    require(shape1.equals(shape2), s"input conflict")
    if (verbose >= 1) logger.info(s"--------initial hardType bitHeap--------\n$bitHeapGroup")
//    logger.info(s"--------initial hardType bitHeap[${hashName(arithInfos)}]--------\n$bitHeapGroup")
    val heapOut = bitHeapGroup.implAllHard(solution)
    logger.info(s"hard output: $heapOut")
    logger.info(s"heapOut width: ${heapOut.width}, nonEmpty width: ${heapOut.heap.count(_.nonEmpty)}")
    logger.info(s"cols : ${heapOut.heights.mkString(",")}")
    if (doFinal3to2) { // TODO: skip 3:2 compressor for columns with height = 2 / 1
      val nonEmptyCols = heapOut.heights.zipWithIndex.filter { case (h, _) => h > 0 }.map(_._2)
      val finalStage = CompressorStageSolution(
        nonEmptyCols.map(i => CompressorStepSolution("Compressor3to2", 1, i)),
        stageInHeight  = 3,
        stageOutHeight = 2,
        pipelined      = true,
        solution.stageSolutions.length + 1
      )
      heapOut.implStageHard(finalStage)
    }
    dataOut := heapOut.toUInts
      .map(_.resize(positiveLength))
      .map(_.toAFix)
  }

  override def doSelfTest(): ChainsawTest = {
    logger.info(s"solution information table: \n${solution.tableReport}")
    logger.info(s"solution vivadoUtils: \n${solution.vivadoUtilEstimation}")
    super.doSelfTest()
  }
}
