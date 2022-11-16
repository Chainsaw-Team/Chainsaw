package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import breeze.numerics.ceil

import scala.collection.mutable.ArrayBuffer

case class WeightedValue(value: BigInt, weight: Int) {
  def eval: BigInt = value << weight

  def <<(shift: Int) = WeightedValue(value, weight + shift)
}

case class KaratsubaAlgo(width: Int, strategy: UtilStrategy, clbPerDsp: Double) { // TODO: rectangular

  val solution = search(width)
  val decompositions = solution.decompositions

  def search(width: Int): KaratsubaSolution = {

    val dsps = Seq((16, 16), (12, 24), (16, 24), (18, 24), (16, 20))

    var bestSolution = KaratsubaSolution((200, 200), Seq(200)) // an extremely large solution
    val undetermined, undeterminedNext = ArrayBuffer[KaratsubaSolution]()
    dsps.foreach(dsp => undetermined += KaratsubaSolution(dsp, Seq[Int]()))

    def iter(solution: KaratsubaSolution): Unit = {
      var splitNext = 2

      def solutionNext: KaratsubaSolution = solution * splitNext

      def isBetter = solutionNext.vivadoUtil.isBetterThan(bestSolution.vivadoUtil, strategy, clbPerDsp)

      // branch
      while (solutionNext.widthFull < width) {
        if (isBetter) undeterminedNext += solutionNext
        splitNext += 1
      }
      if (isBetter) bestSolution = solutionNext
    }

    while (undetermined.nonEmpty) {
      undeterminedNext.clear()
      undetermined.foreach(iter)
      undetermined.clear()
      undeterminedNext.foreach(undetermined += _)
    }
    logger.info(s"$bestSolution")
    bestSolution
  }

  def doNSplitRec(x: BigInt, y: BigInt, decompositions: Seq[Decomposition]): BigInt = {

    if (decompositions.isEmpty) x * y
    else {
      val current = decompositions.head
      import current._

      def doNSplit(aWords: Seq[BigInt], bWords: Seq[BigInt]): Seq[WeightedValue] = {
        val diagonals: Seq[WeightedValue] = (0 until split).map { i =>
          val value = doNSplitRec(aWords(i), bWords(i), decompositions.tail)
          WeightedValue(value, blockWidth * i * 2)
        }

        val prods: Seq[WeightedValue] = Seq.tabulate(split, split) { (i, j) =>
          if (i > j) { // upper triangular
            val fullA = aWords(i) + aWords(j)
            val fullB = bWords(i) + bWords(j)
            val cross = doNSplitRec(fullA, fullB, decompositions.tail) - diagonals(i).value - diagonals(j).value
            Some(WeightedValue(cross, blockWidth * (i + j)))
          } else None
        }.flatten.flatten

        diagonals ++ prods
      }

      val aWords: Seq[BigInt] = x.toBitValue(fullWidth).splitN(aSplit) // words, low to high
      val bWords: Seq[BigInt] = y.toBitValue(fullWidth).splitN(bSplit)

      // distribute words to N-split sub modules
      Seq.tabulate(factorB, factorA) { (i, j) =>
        val as = aWords.zipWithIndex.filter(_._2 % factorB == i).map(_._1)
        val bs = bWords.zipWithIndex.filter(_._2 % factorA == j).map(_._1)
        val weight = i * baseHeight + j * baseWidth
        doNSplit(as, bs).map(_ << weight)
      }.flatten.flatten.map(_.eval).sum
    }
  }

  def impl(x: BigInt, y: BigInt): BigInt = {
    val ret = doNSplitRec(x, y, decompositions)
    //    println(s"yours  $ret\ngolden ${x * y}")
    assert(ret == x * y, s"x $x, y $y, yours $ret, golden ${x * y}")
    ret
  }

}

/** description of a rectangular n-split Karatsuba
 *
 * @param split      number of splits
 * @param baseHeight ''height'' of a basic multiplier
 * @param baseWidth  ''width'' of a basic multiplier
 */
// TODO: cost & count for different Karatsuba Mode - Square, Full, MSB and LSB
case class Decomposition(split: Int, baseHeight: Int, baseWidth: Int) {

  val w = gcd(baseHeight, baseWidth) // common base width
  val factorA = baseHeight / w // factor a
  val factorB = baseWidth / w // factor b

  val aSplit = factorB * split // number of splits, word a
  val bSplit = factorA * split // number of splits, word b

  val fullWidth = split * factorA * factorB * w
  val blockWidth = factorA * factorB * w

  val compressorEff = 1.0

  def widthNext: Int = split * factorA * factorB * w

  def *(split: Int) = Decomposition(split, widthNext, widthNext)

  /** --------
   * costs
   * -------- */
  def subMultCount = split * (split + 1) / 2 * factorA * factorB

  def splitCost: Int = subMultCount * (baseHeight + baseWidth)

  def mergeCost: Double = {
    val minusCost = subMultCount * 2 * (baseHeight + baseWidth) // reduce high & low
    val plusCost = (subMultCount - subMultCount) * (baseHeight + baseWidth) + subMultCount * (baseHeight + baseWidth + 2) - (widthNext * 2) // reduce = all - remained
    (minusCost + plusCost) / compressorEff
  }

  def clbCost: Double = splitCost + mergeCost
}

case class KaratsubaSolution(dsp: (Int, Int), splits: Seq[Int]) {

  // all decompositions
  def head: Decomposition = Decomposition(splits.head, baseHeight = dsp._1, baseWidth = dsp._2)

  def decompositions: Seq[Decomposition] = Seq.iterate((head, 0), splits.length) { case (decomposition, i) => (decomposition * splits(i + 1), i + 1) }
    .map(_._1)

  def layer = splits.length

  def *(split: Int) = KaratsubaSolution(dsp, splits :+ split)

  def widthFull: Int = decompositions.last.widthNext

  def dspCost: Int = decompositions.map(_.subMultCount).product

  def splitCost = decompositions.tails.toSeq.init.map { chain =>
    val multiple = chain.tail.map(_.subMultCount).product
    multiple * chain.head.splitCost
  }.sum

  def mergeCost = decompositions.tails.toSeq.init.map { chain =>
    val multiple = chain.tail.map(_.subMultCount).product
    multiple * chain.head.mergeCost
  }.sum

  def clbCost = decompositions.tails.toSeq.init.map { chain =>
    val multiple = chain.tail.map(_.subMultCount).product
    multiple * chain.head.clbCost
  }.sum

  def vivadoUtil = VivadoUtilRequirement(lut = ceil(clbCost).toInt, dsp = dspCost)

  /** cost when CLB/DSP cost is specified
   *
   * @param clbPerDsp
   * @return
   */
  def ratioCost(clbPerDsp: Double): Double = dspCost.toDouble max (clbCost / clbPerDsp)

  def showCosts(): Unit = {
    println("----Karatsuba report----")
    println(s"layer = $layer")
    println(s"width = $widthFull")
    println(s"dspCost = $dspCost")
    println(s"splitCost = $splitCost")
    println(s"mergeCost = $mergeCost")
    println(s"clbCost = $clbCost")
  }

  override def toString = s"Karatsuba Solution: $widthFull = $dsp -> ${splits.mkString(" -> ")}, dspCost = $dspCost, clbCost = $clbCost"
}