package Chainsaw.arithmetic.bitheap

import Chainsaw._
import Chainsaw.arithmetic._
import spinal.core._

import java.io.File
import scala.language.postfixOps

/** enhanced multi-operand adder implemented by compressors
 */
case class BitHeapCompressor(arithInfos: Seq[ArithInfo], solver: BitHeapSolver = NaiveSolver)
  extends UnsignedMerge {

  override def name = s"BitHeapCompressor_${hashName(arithInfos)}"

  val bitHeapGroup = BitHeapGroup.fromInfos(arithInfos.map(_.toPositive))
  if (compensation > BigInt(0)) bitHeapGroup.addNegativeConstant(-compensation)

  val solutionFile = new File(compressorSolutionDir, s"$name")
  val solution = if (solutionFile.exists()) CompressorFullSolution.load(solutionFile)
  else {
    val solution = solver.solveAll(bitHeapGroup)
    solution.save(solutionFile)
    solution
  }
  logger.info(s"solution cost = ${solution.vivadoUtilEstimation}")

  override def outputTypes = Seq.fill(3)(NumericType.U(maxValue.bitLength))

  override def latency() = solution.latency()

  override def implH = new ChainsawOperatorModule(this) {
    // FIXME: ~ operation should be used outside the bitHeapCompressor
    val operands = dataIn.zip(arithInfos).map { case (fix, info) => if (info.isPositive) fix.asUInt() else ~fix.asUInt() }
    val weightedUInts = operands.zip(arithInfos).map { case (int, info) => WeightedUInt(int, info.toPositive) }
    val bitHeapGroup = BitHeapGroup.fromUInts(weightedUInts)
    if (compensation > BigInt(0)) bitHeapGroup.addNegativeConstant(-compensation)
    dataOut := bitHeapGroup.implAllHard(solution).toUInts.map(_.resize(validLength)).map(_.toAFix)
  }

  override def vivadoUtilEstimation = solution.vivadoUtilEstimation

  override def fmaxEstimation = 600 MHz
}
