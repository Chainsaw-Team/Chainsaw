package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.dag._
import Chainsaw.xilinx._

import scala.language.postfixOps

case class WeightedPort(port: DagPort, arithInfo: ArithInfo) {
  require(port.width <= arithInfo.width)

  def <<(shiftLeft: Int) = WeightedPort(port, arithInfo << shiftLeft)
}

/** hardware implementation of in-depth optimized Karatsuba Algorithm
 *
 * @param width    with of the Karatsuba multiplier
 * @param constant when one of the multiplicands is a constant
 */
case class Karatsuba(width: Int, constant: Option[BigInt] = None,
                     strategy: UtilStrategy, layerMax: Int = 10, clbPerDsp: Double = 1.0)
  extends Dag {

  override def name = s"karatsuba_$width"

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].product)

  val algo = KaratsubaAlgo(width, strategy, layerMax, clbPerDsp)
  logger.info(s"${algo.solution}")

  def getValidWidths(width: Int) = {
    val total = width.divideAndCeil(cpaWidthMax)
    Seq.fill(total - 1)(cpaWidthMax) :+ (width - (total - 1) * cpaWidthMax)
  }

  /** --------
   * primitives used in Karatusba
   * -------- */
  def karaBase(aHigh: DagPort, aLow: DagPort, bHigh: DagPort, bLow: DagPort): Seq[Port] = {
    val kara = KaraBase(aLow.width, bLow.width, Kara).asVertex
    kara := (aHigh, aLow, bHigh, bLow)
    kara.outPorts
  }

  def merge(operands: Seq[WeightedPort]) = {
    val treeGen = BitHeapCompressor(operands.map(_.arithInfo), outputAsCsa = false)
    val merge = treeGen.asVertex
    merge := (operands.map(_.port): _*)
    merge.out(0)
  }

  def getTiling(a: DagPort, b: DagPort) = {
    val core = GetTiling(a.width - 1, b.width - 1).asVertex
    core := (a, b)
    core.outPorts
  }

  /** --------
   * graph construction
   * -------- */
  // TODO: more karatsuba construction method including LSB\MSB\SQUARE

  val Seq(a, b) = Seq.fill(2)(InputVertex(UIntInfo(width))).map(_.resize(algo.solution.widthFull))
  val o = OutputVertex(UIntInfo(width * 2))

  def doNSplitRec(x: DagPort, y: DagPort, decompositions: Seq[Decomposition]): DagPort = {

    val current = decompositions.last
    import current._

    val aWords = x.splitN(aSplit) // width = factorB
    val bWords = y.splitN(bSplit) // width = factorA

    if (decompositions.length == 1) { // last layer optimized by dedicated DSP datapath
      val segments: Seq[WeightedPort] = Seq.tabulate(factorB, factorA) { (i, j) =>
        // distribute words to N-split sub modules
        val as = aWords.zipWithIndex.filter(_._2 % factorB == i).map(_._1)
        val bs = bWords.zipWithIndex.filter(_._2 % factorA == j).map(_._1)
        require(as.length == 2 && bs.length == 2) // the last layer must be 2-split
        val Seq(aL, aH) = as
        val Seq(bL, bH) = bs
        val Seq(high, cross, low) = karaBase(aH, aL, bH, bL)
        val operands = Seq(
          // as diagonal
          WeightedPort(high, ArithInfo(width = segmentWidth, weight = widthBlock * 2)),
          WeightedPort(low, ArithInfo(width = segmentWidth, weight = widthBlock * 0)),
          // as kara
          WeightedPort(cross, ArithInfo(width = segmentWidth + 2, weight = widthBlock * 1)),
        )
        val weight = i * baseHeight + j * baseWidth
        operands.map(_ << weight)
      }.flatten.flatten
      merge(segments)
    }
    else { // other layers which won't directly use DSP
      val segmentWidth = baseWidth + baseHeight

      def doNSplit(aWords: Seq[DagPort], bWords: Seq[DagPort]) = {

        if (segmentWidth == 96) logger.warn("here 96")

        val diagonals = (0 until split).map { i =>
          val value = doNSplitRec(aWords(i), bWords(i), decompositions.init)
          WeightedPort(value, ArithInfo(width = segmentWidth, weight = widthBlock * i * 2))
        }

        val prods = Seq.tabulate(split, split) { (i, j) =>
          if (i > j) { // upper triangular

            val mergedA = aWords(i) +^ aWords(j)
            val mergedB = bWords(i) +^ bWords(j)

            val Seq(aMain, bMain, sideA, sideB, ab) = getTiling(mergedA, mergedB)
            val full = doNSplitRec(aMain, bMain, decompositions.init)

            val sA = WeightedPort(sideA, ArithInfo(width = baseWidth, weight = widthBlock * (i + j) + baseHeight))
            val sB = WeightedPort(sideB, ArithInfo(width = baseHeight, weight = widthBlock * (i + j) + baseWidth))
            val AB = WeightedPort(ab, ArithInfo(width = 1, weight = widthBlock * (i + j) + segmentWidth))
            val cross = WeightedPort(full, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j)))
            val high = WeightedPort(diagonals(i).port, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j), isPositive = false))
            val low = WeightedPort(diagonals(j).port, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j), isPositive = false))

            Some(Seq(cross, high, low, sA, sB, AB))
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

      val ret = merge(segments)
      ret
    }
  }

  o := doNSplitRec(a, b, algo.decompositions).resize(width * 2)

  this.exportPng(s"kara$width")

  graphDone()

  override def implNaiveH = Some(new ChainsawModule(this) {
    if (constant.nonEmpty) uintDataOut.head := (uintDataIn.head * constant.get).d(latency)
    else uintDataOut.head := uintDataIn.reduce(_ * _).d(latency)
  })
}