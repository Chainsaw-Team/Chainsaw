package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._

import scala.collection.mutable.ArrayBuffer

case class WeightedValue(value: BigInt, arithInfo: ArithInfo) {
  def eval: BigInt = {
    assert(value.bitLength <= arithInfo.width, s"yours: ${value.bitLength}, upper: ${arithInfo.width}")
    (value << arithInfo.weight) * (if (arithInfo.isPositive) 1 else -1)
  }

  def <<(shiftLeft: Int) = WeightedValue(value, arithInfo << shiftLeft)
}

case class KaratsubaAlgo(width: Int, strategy: UtilStrategy, layerMax: Int = 10, clbPerDsp: Double = 1.0) { // TODO: rectangular

  val solution = search(width)
  val decompositions = solution.decompositions

  /** get the best Karatsuba Solution by branch and bound algorithm
   *
   * @param width width of the multiplier
   */
  def search(width: Int): KaratsubaSolution = {

    val dsps = Seq((16, 16), (12, 24), (16, 24), (16, 20))

    var bestSolution = KaratsubaSolution((200, 200), Seq(200)) // an extremely large solution
    val undetermined, undeterminedNext = ArrayBuffer[KaratsubaSolution]()

    // init
    dsps.foreach { dsp =>
      val solution = KaratsubaSolution(dsp, Seq(2))
      if (solution.widthFull >= width) {
        if (solution.vivadoUtil.isBetterThan(bestSolution.vivadoUtil, strategy, clbPerDsp)) bestSolution = solution
      }
      else undetermined += solution
    }

    def iter(solution: KaratsubaSolution): Unit = {

      var isKara = true
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

    var layerNext = 1
    while (undetermined.nonEmpty && layerNext <= layerMax) {
      undeterminedNext.clear()
      undetermined.foreach(iter)
      undetermined.clear()
      undeterminedNext.foreach(undetermined += _)
      layerNext += 1
    }

    logger.info(s"$bestSolution\nstrategy: ${strategy.getClass.getSimpleName}, layerMax: $layerMax")
    bestSolution
  }

  /** --------
   * cost recording
   * -------- */
  var multCost = 0
  var splitCost = 0
  var mergeCost = 0

  def clearCost(): Unit = {
    multCost = 0
    splitCost = 0
    mergeCost = 0
  }

  def add(v0: BigInt, v1: BigInt, width: Int): BigInt = {
    assert(v0.bitLength <= width && v1.bitLength <= width)
    splitCost += width
    v0 + v1
  }

  def mult(v0: BigInt, v1: BigInt): BigInt = {
    assert(Seq(v0.bitLength, v1.bitLength).sorted.zip(Seq(16, 25)).forall { case (yours, upper) => yours <= upper }, s"${v0.bitLength}, ${v1.bitLength}")
    multCost += 1
    v0 * v1
  }

  def merge(widthOut: Int, weightedValues: Seq[WeightedValue]): BigInt = {
    weightedValues.foreach(weightedValue => assert(weightedValue.value.bitLength <= weightedValue.arithInfo.width))
    //    println(weightedValues.map(_.arithInfo.weight).mkString(" "))
    mergeCost += weightedValues.map(_.arithInfo.width).sum - widthOut
    weightedValues.map(_.eval).sum
  }



  /** --------
   * algo
   * -------- */
  def doNSplitRec(x: BigInt, y: BigInt, decompositions: Seq[Decomposition]): BigInt = {
    val costBefore = (splitCost, mergeCost)
    if (decompositions.isEmpty) mult(x, y)
    else {

      val current = decompositions.last
      import current._

      val segmentWidth = baseWidth + baseHeight
      val widthOut = widthFull

      val aWords: Seq[BigInt] = x.toBitValue(widthFull).splitN(aSplit) // width = baseHeight
      val bWords: Seq[BigInt] = y.toBitValue(widthFull).splitN(bSplit) // width = baseWidth

      def doNSplit(aWords: Seq[BigInt], bWords: Seq[BigInt]): Seq[WeightedValue] = {
        val diagonals: Seq[WeightedValue] = (0 until split).map { i =>
          val value = doNSplitRec(aWords(i), bWords(i), decompositions.init)
          WeightedValue(value, ArithInfo(width = segmentWidth, weight = widthBlock * i * 2))
        }

        val prods: Seq[WeightedValue] = Seq.tabulate(split, split) { (i, j) =>
          if (i > j) { // upper triangular

            val mergedA = add(aWords(i), aWords(j), baseHeight)
            val mergedB = add(bWords(i), bWords(j), baseWidth)

            //            val full = doNSplitRec(mergedA, mergedB, decompositions.init)

            val (aMsb, aMain) = mergedA.toBitValue(baseWidth + 1).splitAt(baseHeight)
            val (bMsb, bMain) = mergedB.toBitValue(baseHeight + 1).splitAt(baseWidth)
            val full = doNSplitRec(aMain, bMain, decompositions.init)
            val sideA = WeightedValue(aMsb * bMain, ArithInfo(width = baseWidth, weight = widthBlock * (i + j) + baseHeight))
            val sideB = WeightedValue(bMsb * aMain, ArithInfo(width = baseHeight, weight = widthBlock * (i + j) + baseWidth))
            val ab = WeightedValue(aMsb * bMsb, ArithInfo(width = 1, weight = widthBlock * (i + j) + segmentWidth))

            val cross = WeightedValue(full, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j)))

            val high = WeightedValue(diagonals(i).value, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j), isPositive = false))
            val low = WeightedValue(diagonals(j).value, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j), isPositive = false))
            Some(Seq(cross, high, low))
            Some(Seq(cross, high, low, sideA, sideB, ab))
          } else None
        }.flatten.flatten.flatten

        diagonals ++ prods
      }

      val segments = Seq.tabulate(factorB, factorA) { (i, j) =>
        // distribute words to N-split sub modules
        val as = aWords.zipWithIndex.filter(_._2 % factorB == i).map(_._1)
        val bs = bWords.zipWithIndex.filter(_._2 % factorA == j).map(_._1)
        val weight = i * baseHeight + j * baseWidth
        doNSplit(as, bs).map(_ << weight)
      }.flatten.flatten

      val ret = merge(widthOut, segments)
      ret
    }
  }

  def impl(x: BigInt, y: BigInt): BigInt = {
    clearCost()
    val ret = doNSplitRec(x, y, decompositions)
    assert(multCost == solution.dspCost, s"estimated: ${solution.dspCost}, actual: $multCost")
    assert(splitCost == solution.splitCost, s"estimated: ${solution.splitCost}, actual: $splitCost")
    assert(mergeCost == solution.mergeCost, s"estimated: ${solution.mergeCost}, actual: $mergeCost")
    assert(ret == x * y, s"x $x, y $y, yours $ret, golden ${x * y}")
    ret
  }

}