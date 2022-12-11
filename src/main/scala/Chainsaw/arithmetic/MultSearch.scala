package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.deprecated.Bcm
import Chainsaw.xilinx.{VivadoUtil, VivadoUtilRequirement}
import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** unified multiplier design interface, following algos are supported:
 *          - 1. divide-and-conquer based implementation of big multiplier
 *            [[BmAlgo]], [[Bm]]
 *
 *          - 2. bit heap compression based implementation of big multiplier
 *            [[BcmAlgo]] [[Bcm]]
 *
 *          - 3. RNS-based implementation of mid multiplier
 *
 *          - 4. look up table based implementation of small multiplier
 */
object MultSearch {

  def apply(width: Int, multiplierType: MultiplierType, budget: VivadoUtil): Unit = {
    val solutions = getBmParetos(width, multiplierType)
    val count = budget.solveBestScheme(solutions.map(_.vivadoUtil + VivadoUtilRequirement(lut = 20000, dsp = 0)), Seq(0, 2)) // 0 & 2 for LUT & DSP
    count.zip(solutions).foreach { case (i, solution) => logger.info(s"$i X $solution") }
    val util = count.zip(solutions).map { case (i, solution) => solution.vivadoUtil * i }.reduce(_ + _)
    logger.info(s"LUT: ${util.lut} / ${budget.lut}, DSP: ${util.dsp} / ${budget.dsp}")
  }

  // available starting points
  // FIXME: 17 X 17 leads to problem, why?
  val baseMultipliers = Seq(
    BaseDspMult(16, 16), BaseDspMult(12, 24), BaseDspMult(16, 24), BaseDspMult(16, 20), // use dsp as pure multiplier
    Sm(FullMultiplier), // taking advantage of dsps' pre-adder/post-adder
  )

  /** get the best BmSolutions for variable multiplications by branch and bound + dynamic programming algorithm
   *
   * @param width width of the multiplier
   */
  // TODO: search for MSB/LSB multipliers, with optimized base multipliers(LSB/MSB version)
  def getBmParetos(width: Int, multiplierType: MultiplierType): Seq[BmSolution] = {

    val solutionSet = mutable.Map[Int, ArrayBuffer[BmSolution]]() // width -> solutions

    def inferior(a: BmSolution, b: BmSolution): Boolean = // definition of Pareto
      a.vivadoUtil.lut >= b.vivadoUtil.lut && a.vivadoUtil.dsp >= b.vivadoUtil.dsp

    def makePareto(solutions: ArrayBuffer[BmSolution]): mutable.Seq[BmSolution] = {
      val n = solutions.length
      val inferiors = ArrayBuffer[BmSolution]()
      Seq.tabulate(n, n) { (i, j) =>
        if (i != j) if (inferior(solutions(i), solutions(j))) {
          if (solutions(i).vivadoUtil == solutions(j).vivadoUtil) inferiors += solutions(i max j) // always drop the smaller one
          else inferiors += solutions(i)
        }
      }
      inferiors.foreach(solutions -= _)
      solutions
    }

    val naiveSolution = {
      val stage = log2Up((width + 15) / 16)
      BmSolution(baseMultipliers.head, Seq.fill(stage)(2), multiplierType, Seq.fill(stage)(true))
      //      BmSolution(baseMultipliers.head, Seq.fill(stage)(2), multiplierType, Seq.fill(stage)(false))
    }

    solutionSet(naiveSolution.widthFull) = ArrayBuffer[BmSolution]()
    solutionSet(naiveSolution.widthFull) += naiveSolution

    def reduceSolutions(): Unit = solutionSet.values.foreach(makePareto)

    def addSolution(solutionNext: BmSolution): Unit = {
      if (!inferior(solutionNext, naiveSolution)) {
        val widthFull = solutionNext.widthFull
        if (!solutionSet.isDefinedAt(widthFull)) solutionSet(widthFull) = ArrayBuffer[BmSolution]()
        solutionSet(solutionNext.widthFull) += solutionNext
      }
    }

    // initialization
    baseMultipliers.foreach { dsp =>
      if (dsp.widthX == dsp.widthY) addSolution(BmSolution(dsp, Seq[Int](), multiplierType, Seq[Boolean]()))
      else {
        (2 until 100).map(i => BmSolution(dsp, Seq(i), multiplierType, Seq(true))).filter(_.widthFull <= width).foreach(addSolution)
        (2 until 100).map(i => BmSolution(dsp, Seq(i), multiplierType, Seq(false))).filter(_.widthFull <= width).foreach(addSolution)
      }
    }

    def iter(solution: BmSolution): Unit = {
      var splitNext = 2

      def solutionsNext = {
        val a = solution.expand(splitNext, isKara = true)
        val b = solution.expand(splitNext, isKara = false)
        if (multiplierType == FullMultiplier) Seq(a, b) else Seq(a)
      }

      while (solutionsNext.head.widthFull < width) {
        solutionsNext.foreach(addSolution)
        splitNext += 1
      }
      solutionsNext.foreach(addSolution)
    }

    def undetermined = solutionSet.filter { case (i, solutions) => i < width && solutions.nonEmpty }

    while (undetermined.nonEmpty) {
      val old = mutable.Map[Int, ArrayBuffer[BmSolution]]()
      undetermined.foreach { case (i, solutions) => solutions // copy to old
        .foreach { solution =>
          if (!old.isDefinedAt(i)) old(i) = ArrayBuffer[BmSolution]()
          old(i) += solution
        }
      }

      undetermined.foreach(_._2.foreach(iter))
      reduceSolutions()
      old.foreach { case (i, solutions) => solutions.foreach(solutionSet(i) -= _) } // drop old
    }

    val all = ArrayBuffer(solutionSet.filter(_._1 >= width).flatMap(_._2).toSeq: _*)
    val ret = makePareto(all)
    logger.info(s"Pareto solutions for $width-bit ${className(multiplierType)}:\n${ret.mkString("\n")}")
    ret
  }

  // TODO: search for constant multipliers with dynamic threshold

}
