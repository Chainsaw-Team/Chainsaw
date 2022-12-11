package Chainsaw.deprecated

import Chainsaw.arithmetic.{ArithInfo, BmAlgo, BmSolution, cpaWidthMax}
import Chainsaw._

import scala.util.Random
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

/** hardware implementation of in-depth optimized Karatsuba Algorithm
 *
 * @param width    with of the Karatsuba multiplier
 * @param constant when one of the multiplicands is a constant
 */
case class Bm(width: Int, constant: Option[BigInt] = None, bmSolution: BmSolution)
  extends Dag {

  case class WeightedPort(port: DagPort, arithInfo: ArithInfo) {
    require(port.width <= arithInfo.width, s"port width: ${port.width}, info width: ${arithInfo.width}")

    def <<(shiftLeft: Int) = WeightedPort(port, arithInfo << shiftLeft)
  }


  override def generateTestCases = Seq.fill(1000)(BigInt(width, Random))

  override def name = getAutoName(this)

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].product)

  val algo = BmAlgo(bmSolution)
  logger.info(s"${algo.bmSolution}")

  def getValidWidths(width: Int) = {
    val total = width.divideAndCeil(cpaWidthMax)
    Seq.fill(total - 1)(cpaWidthMax) :+ (width - (total - 1) * cpaWidthMax)
  }

  /** --------
   * primitives used in Karatusba
   * -------- */
  def karaBase(aHigh: DagPort, aLow: DagPort, bHigh: DagPort, bLow: DagPort): Seq[Port] = {
    val kara = deprecated.DedicatedMult(aLow.width, bLow.width, Kara).asVertex
    kara := (aHigh, aLow, bHigh, bLow)
    kara.outPorts
  }

  def mult4(aHigh: DagPort, aLow: DagPort, bHigh: DagPort, bLow: DagPort): Port = {
    val kara = deprecated.DedicatedMult(aLow.width, bLow.width, FullMultiplier).asVertex
    kara := (aHigh, aLow, bHigh, bLow)
    kara.outPorts.head
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

  val Seq(a, b) = Seq.fill(2)(InputVertex(UIntInfo(width))).map(_.resize(algo.bmSolution.widthFull))
  val o = OutputVertex(UIntInfo(width * 2))

  def doNSplitRec(x: DagPort, y: DagPort, solution: BmSolution): Port = {

    val current = solution.topDecomposition
    import current._

    val aWords = x.splitN(aSplit) // width = factorB
    val bWords = y.splitN(bSplit) // width = factorA

    val ret: Port = if (solution.length == 1) { // last layer optimized by dedicated DSP datapath
      require(split == 2) // split must be 2
      multiplierType match {
        case FullMultiplier => if (isKara) {
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
          merge(segments).resize(widthNext * 2)
        } else {
          val prods = Seq.tabulate(aSplit / 2, bSplit / 2) { (i, j) =>
            val (aH, aL) = (aWords(2 * i + 1), aWords(2 * i + 0))
            val (bH, bL) = (bWords(2 * j + 1), bWords(2 * j + 0))
            val prod = mult4(aH, aL, bH, bL)
            WeightedPort(prod, ArithInfo(segmentWidth * 2, weight = segmentWidth * 2 * (i + j)))
          }.flatten
          merge(prods)
        }
        case SquareMultiplier =>
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
          merge(segments).resize(widthNext * 2)
        //          require(aSplit == 2 && bSplit == 2)
        //          val base = KaraBase(baseHeight, baseWidth, SquareMultiplier).asVertex
        //          base := (aWords(1), aWords(0), bWords(1), bWords(0))
        //          base.out(0)
        case MsbMultiplier => ???
        case LsbMultiplier => ???
      }
    }
    else {
      multiplierType match {
        case FullMultiplier => if (isKara) {
          // other layers which won't directly use DSP
          val segmentWidth = baseWidth + baseHeight

          def doNSplit(aWords: Seq[DagPort], bWords: Seq[DagPort]) = {
            val diagonals = (0 until split).map { i =>
              println(solution)
              println(aWords(i).width)
              println(bWords(i).width)
              val value = doNSplitRec(aWords(i), bWords(i), solution.subSolution(multiplierType))
              WeightedPort(value, ArithInfo(width = segmentWidth, weight = widthBlock * i * 2))
            }

            val prods = Seq.tabulate(split, split) { (i, j) =>
              if (i > j) { // upper triangular

                val mergedA = aWords(i) +^ aWords(j)
                val mergedB = bWords(i) +^ bWords(j)

                val Seq(aMain, bMain, sideA, sideB, ab) = getTiling(mergedA, mergedB)
                val full = doNSplitRec(aMain, bMain, solution.subSolution(FullMultiplier))

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

          val ret = merge(segments).resize(widthNext * 2)
          ret
        } else {
          val segmentWidth = baseWidth + baseHeight

          def doNSplit(aWords: Seq[DagPort], bWords: Seq[DagPort]) = {
            Seq.tabulate(split, split) { (i, j) =>
              val value = doNSplitRec(aWords(i), bWords(j), solution.subSolution(FullMultiplier))
              WeightedPort(value, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j)))
            }.flatten
          }

          val segments = Seq.tabulate(factorB, factorA) { (i, j) =>
            // distribute words to N-split sub modules
            val as = aWords.zipWithIndex.filter(_._2 % factorB == i).map(_._1)
            val bs = bWords.zipWithIndex.filter(_._2 % factorA == j).map(_._1)
            val weight = i * baseHeight + j * baseWidth
            doNSplit(as, bs).map(_ << weight)
          }.flatten.flatten
          val ret = merge(segments).resize(widthNext * 2)
          ret
        }
        case SquareMultiplier =>
          val segmentWidth = baseWidth + baseHeight

          def doNSplit(aWords: Seq[DagPort], bWords: Seq[DagPort]) = {
            val diagonals = (0 until split).map { i =>
              val value = doNSplitRec(aWords(i), bWords(i), solution.subSolution(multiplierType))
              WeightedPort(value, ArithInfo(width = segmentWidth, weight = widthBlock * i * 2))
            }

            val prods = Seq.tabulate(split, split) { (i, j) =>
              if (i > j) { // upper triangular
                val prod = doNSplitRec(aWords(i), bWords(j), solution.subSolution(FullMultiplier))
                Some(WeightedPort(prod, ArithInfo(width = segmentWidth, weight = widthBlock * (i + j) + 1)))
              } else None
            }.flatten.flatten

            diagonals ++ prods
          }

          val segments = Seq.tabulate(factorB, factorA) { (i, j) =>
            // distribute words to N-split sub modules
            val as = aWords.zipWithIndex.filter(_._2 % factorB == i).map(_._1)
            val bs = bWords.zipWithIndex.filter(_._2 % factorA == j).map(_._1)
            val weight = i * baseHeight + j * baseWidth
            doNSplit(as, bs).map(_ << weight)
          }.flatten.flatten

          val ret = merge(segments).resize(widthNext * 2)
          ret
        case MsbMultiplier => ???
        case LsbMultiplier => ???
      }
    }
    ret
  }

  o := doNSplitRec(a, b, algo.bmSolution).resize(width * 2)

  this.exportPng(s"bm$width")

  graphDone()

  override def implNaiveH = Some(new ChainsawModule(this) {
    if (constant.nonEmpty) uintDataOut.head := (uintDataIn.head * constant.get).d(latency)
    else uintDataOut.head := uintDataIn.reduce(_ * _).d(latency)
  })
}
