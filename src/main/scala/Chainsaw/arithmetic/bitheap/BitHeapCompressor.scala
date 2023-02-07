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
    solver: BitHeapSolver = GreedSolver
) extends UnsignedMerge {

  override def name =
    s"BitHeapCompressor_${hashName(arithInfos)}_${className(solver)}"

  val bitHeapGroup            = BitHeapGroup.fromInfos(arithInfos)
  override val positiveLength = bitHeapGroup.positiveLength

  val solutionFile = new File(compressorSolutionOutputDir, s"$name")
  val solution = if (solutionFile.exists()) {
    logger.info(s"take existing solution from $solutionFile")
    CompressorFullSolution.load(solutionFile)
  } else {
    val solution = solver.solveAll(bitHeapGroup)
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

  override def latency() = solution.latency

  override def implH = new ChainsawOperatorModule(this) {
    val operands = dataIn.map(_.asUInt())
    val weightedUInts = operands.zip(arithInfos).map { case (int, info) =>
      WeightedUInt(int, info)
    }
    val bitHeapGroup = BitHeapGroup.fromUInts(weightedUInts)
    val heapOut      = bitHeapGroup.implAllHard(solution)
    if (doFinal3to2) { // TODO: skip 3:2 compressor for columns with height = 2 / 1
      val finalStage = CompressorStageSolution(
        (0 until heapOut.width).map(i =>
          CompressorStepSolution("Compressor3to2", 1, i)
        ),
        stageHeight = 2,
        pipelined   = false
      )
      heapOut.implStageHard(finalStage)
    }
    dataOut := heapOut.toUInts
      .map(_.resize(positiveLength))
      .map(_.toAFix)
  }
}
