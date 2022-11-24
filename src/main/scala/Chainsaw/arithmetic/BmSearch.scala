package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx.{VivadoUtil, VivadoUtilRequirement}
import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object BmSearch {

  def apply(width: Int, multiplierType: MultiplierType, budget: VivadoUtil): Unit = {
    val solutions = getParetos(width, multiplierType)
    val count = budget.solveBestScheme(solutions.map(_.vivadoUtil + VivadoUtilRequirement(lut = 20000, dsp = 0)), Seq(0, 2)) // 0 & 2 for LUT & DSP
    count.zip(solutions).foreach { case (i, solution) => logger.info(s"$i X $solution") }
    val util = count.zip(solutions).map { case (i, solution) => solution.vivadoUtil * i }.reduce(_ + _)
    logger.info(s"LUT: ${util.lut} / ${budget.lut}, DSP: ${util.dsp} / ${budget.dsp}")
  }

  /** get the best Karatsuba Solution by branch and bound algorithm
   *
   * @param width width of the multiplier
   */
  def getParetos(width: Int, multiplierType: MultiplierType): Seq[BmSolution] = {

    val dsps =
    //      if (multiplierType == FullMultiplier) Seq((16, 16), (12, 24), (16, 24), (16, 20))
      Seq((16, 16), (12, 24), (16, 24), (16, 20))


    val solutionSet = mutable.Map[Int, ArrayBuffer[BmSolution]]()

    def inferior(a: BmSolution, b: BmSolution): Boolean =
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
      BmSolution((16, 16), Seq.fill(stage)(2), multiplierType, Seq.fill(stage)(true))
    }

    solutionSet(naiveSolution.widthFull) = ArrayBuffer[BmSolution]()
    solutionSet(naiveSolution.widthFull) += naiveSolution

    def updateSolution(): Unit = solutionSet.values.foreach(makePareto)

    def addSolution(solutionNext: BmSolution): Unit = {
      if (!inferior(solutionNext, naiveSolution)) {
        val widthFull = solutionNext.widthFull
        if (!solutionSet.isDefinedAt(widthFull)) solutionSet(widthFull) = ArrayBuffer[BmSolution]()
        solutionSet(solutionNext.widthFull) += solutionNext
      }
    }

    // init
    dsps.foreach { dsp => // first layer decompositions are special
      // val a = BmSolution(dsp, Seq(2), multiplierType, Seq(true))
      // val b = BmSolution(dsp, Seq(2), multiplierType, Seq(false))
      // val solutions = if (multiplierType == FullMultiplier) Seq(a, b) else Seq(a)
      // solutions.foreach(addSolution)
      // must be Kara at the first layer
      addSolution(BmSolution(dsp, Seq(2), multiplierType, Seq(true)))
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

    def determined = solutionSet.filter { case (i, solutions) => i >= width && solutions.nonEmpty }

    while (undetermined.nonEmpty) {
      val old = mutable.Map[Int, ArrayBuffer[BmSolution]]()
      undetermined.foreach { case (i, solutions) => solutions // copy to old
        .foreach { solution =>
          if (!old.isDefinedAt(i)) old(i) = ArrayBuffer[BmSolution]()
          old(i) += solution
        }
      }

      undetermined.foreach(_._2.foreach(iter))
      updateSolution()
      old.foreach { case (i, solutions) => solutions.foreach(solutionSet(i) -= _) } // drop old
    }

    val all = ArrayBuffer(solutionSet.filter(_._1 >= width).flatMap(_._2).toSeq: _*)
    val ret = makePareto(all)
    if (verbose >= 1) logger.info(s"solutions for $width bit big multiplier:\n\t${ret.mkString("\n\t")}")
    ret
  }
}
