package Chainsaw.arithmetic

import Chainsaw._

import scala.util.Random

case class BmAlgo(bmSolution: BmSolution) {

  case class WeightedValue(value: BigInt, arithInfo: ArithInfo) {
    def eval: BigInt = {
      assert(value.bitLength <= arithInfo.width, s"yours: ${value.bitLength}, upper: ${arithInfo.width}")
      (value << arithInfo.weight) * (if (arithInfo.isPositive) 1 else -1)
    }

    def <<(shiftLeft: Int) = WeightedValue(value, arithInfo << shiftLeft)
  }

  logger.info(bmSolution.toString)

  def impl(x: BigInt, y: BigInt): BigInt = {
    val ret = doNSplitRec(x, y, bmSolution)

    val golden = bmSolution.multiplierType match {
      case FullMultiplier => x * y
      case SquareMultiplier => x * x
      case MsbMultiplier => ???
      case LsbMultiplier => ???
    }

    assert(ret == golden, s"x $x, y $y, yours $ret, golden $golden")
    assert(multCost == bmSolution.dspCost, s"estimated: ${bmSolution.dspCost}, actual: $multCost")
    assert(splitCost == bmSolution.splitCost, s"estimated: ${bmSolution.splitCost}, actual: $splitCost")
    assert(mergeCost == bmSolution.mergeCost, s"estimated: ${bmSolution.mergeCost}, actual: $mergeCost")
    //    println(s"x $x, y $y, yours $ret, golden ${x * y}")
    clearCost()
    ret
  }

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
    mergeCost += weightedValues.map(_.arithInfo.width).sum - widthOut
    weightedValues.map(_.eval).sum
  }

  def doNSplitRec(x: BigInt, y: BigInt, bmSolution: BmSolution): BigInt = {
    if (bmSolution.isEmpty) mult(x, y)
    else {
      val current = bmSolution.topDecomposition
      import current._
      val segmentWidth = baseWidth + baseHeight

      val aWords: Seq[BigInt] = x.toBitValue(widthNext).splitN(aSplit) // width = baseHeight
      val bWords: Seq[BigInt] = y.toBitValue(widthNext).splitN(bSplit) // width = baseWidth

      def doNSplit(aWords: Seq[BigInt], bWords: Seq[BigInt]): Seq[WeightedValue] = {

        multiplierType match {
          case FullMultiplier =>
            if (isKara) {
              val diagonals: Seq[WeightedValue] = (0 until split).map { i =>
                // sub mults on diagonal inherit the multType
                val value = doNSplitRec(aWords(i), bWords(i), bmSolution.subSolution(bmSolution.multiplierType))
                WeightedValue(value, ArithInfo(width = segmentWidth, weight = widthBlock * i * 2))
              }

              val prods: Seq[WeightedValue] = Seq.tabulate(split, split) { (i, j) =>
                if (i > j) { // upper triangular

                  val mergedA = add(aWords(i), aWords(j), baseHeight)
                  val mergedB = add(bWords(i), bWords(j), baseWidth)

                  val (aMsb, aMain) = mergedA.toBitValue(baseWidth + 1).splitAt(baseHeight)
                  val (bMsb, bMain) = mergedB.toBitValue(baseHeight + 1).splitAt(baseWidth)

                  val full = doNSplitRec(aMain, bMain, bmSolution.subSolution(FullMultiplier))

                  val sideA = WeightedValue(aMsb * bMain, ArithInfo(width = baseWidth, weight = widthBlock * (i + j) + baseHeight))
                  val sideB = WeightedValue(bMsb * aMain, ArithInfo(width = baseHeight, weight = widthBlock * (i + j) + baseWidth))
                  val ab = WeightedValue(aMsb * bMsb, ArithInfo(width = 1, weight = widthBlock * (i + j) + segmentWidth))

                  val cross = WeightedValue(full, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j)))
                  val high = WeightedValue(diagonals(i).value, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j), isPositive = false))
                  val low = WeightedValue(diagonals(j).value, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j), isPositive = false))

                  Some(Seq(cross, high, low, sideA, sideB, ab))
                } else None
              }.flatten.flatten.flatten

              diagonals ++ prods
            } else {
              Seq.tabulate(split, split) { (i, j) =>
                val value = doNSplitRec(aWords(i), bWords(j), bmSolution.subSolution(bmSolution.multiplierType))
                WeightedValue(value, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j)))
              }.flatten
            }

          case SquareMultiplier =>
            val diagonals: Seq[WeightedValue] = (0 until split).map { i =>
              // sub mults on diagonal inherit the multType
              val value = doNSplitRec(aWords(i), bWords(i), bmSolution.subSolution(bmSolution.multiplierType))
              WeightedValue(value, ArithInfo(width = segmentWidth, weight = widthBlock * i * 2))
            }
            val prods: Seq[WeightedValue] = Seq.tabulate(split, split) { (i, j) =>
              if (i > j) { // upper triangular
                val value = doNSplitRec(aWords(i), bWords(j), bmSolution.subSolution(FullMultiplier))
                Some(WeightedValue(value, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j) + 1)))
              } else None
            }.flatten.flatten
            diagonals ++ prods

          case MsbMultiplier => ???
          case LsbMultiplier => ???
          case Kara => ???
        }
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

  def selfTest(): Unit = {
    def getMultiplicand = BigInt(bmSolution.widthFull, Random)

    val data = if (bmSolution.multiplierType == SquareMultiplier) Seq.fill(1000)(getMultiplicand).map(x => (x, x))
    else Seq.fill(1000)(getMultiplicand, getMultiplicand)
    data.foreach { case (x, y) => impl(x, y) }
    logger.info("bm algo test passed")
  }
}
