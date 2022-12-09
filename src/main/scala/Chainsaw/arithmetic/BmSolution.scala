package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx.VivadoUtilRequirement
import breeze.numerics.ceil

/**
 *
 * @param dsp            size of embedded multiplier
 * @param splits         number of splits for each layer, bottom-up
 * @param multiplierType multiplication target
 * @param isKaras        use Karatsuba/School book BmDecomposition for each layer, bottom-up
 * @param constant       Some(constant) if the multiplication is constant, None otherwise
 * @param threshold      weight for a multiplication to use a dsp
 */
case class BmSolution(dsp: (Int, Int),
                      splits: Seq[Int],
                      multiplierType: MultiplierType,
                      isKaras: Seq[Boolean],
                      constant: Option[BigInt] = None,
                      threshold: Int = 0)
  extends ChainsawSolution {

  require(splits.length == isKaras.length)

  val length = splits.length
  val isEmpty = length == 0
  val layerCount: Int = splits.length

  /** baseWidth of each layer in bottom-up order
   */
  lazy val allBaseWidths: Seq[(Int, Int)] = Seq.iterate((dsp, 0), layerCount) { case (width, i) =>
    val widthNext = BmDecomposition.widthNext(width, splits(i))
    ((widthNext, widthNext), i + 1)
  }.map(_._1)

  lazy val topDecomposition = BmDecomposition(allBaseWidths.last, splits.last, multiplierType, isKaras.last)

  /** tree of all BmDecompositions in top-down order, each Seq contains BmDecompositions in a layer
   */
  lazy val allBmDecompositions: Seq[Seq[BmDecomposition]] = {
    Seq.iterate((Seq(topDecomposition), 1), layerCount) { case (decomposition, i) =>
      val split = splits.reverse(i)
      val isKara = isKaras.reverse(i)
      val baseSize = allBaseWidths.reverse(i)
      val next = decomposition.flatMap(_.typesNext.map(BmDecomposition(baseSize, split, _, isKara)))
      (next, i + 1)
    }.map(_._1)
  }

  lazy val widthFull: Int = topDecomposition.widthNext
  lazy val widthOut: Int = widthFull * 2

  lazy val compressorEff = 1.0 // TODO: vary for different sizes

  lazy val dspCost: Int = allBmDecompositions.last.map(_.subMultCount).sum

  lazy val splitCost: Int = allBmDecompositions.flatten.map(_.splitCost).sum

  lazy val mergeCost: Double = allBmDecompositions.flatten.map(_.mergeCost).sum / compressorEff

  lazy val clbCost: Double = splitCost + mergeCost

  lazy val bitCost: Double = clbCost + (dspCost * (dsp._1 * dsp._2 - dsp._1 - dsp._2) / compressorEff)

  lazy val vivadoUtil = VivadoUtilRequirement(lut = ceil(clbCost).toInt, dsp = dspCost)

  /** --------
   * operations
   * -------- */

  def expand(split: Int, isKara: Boolean) = BmSolution(dsp, splits :+ split, multiplierType, isKaras :+ isKara)

  def subSolution(typeSub: MultiplierType) = BmSolution(dsp, splits.init, typeSub, isKaras.init)

  def showCosts(): Unit = {
    println("----Karatsuba report----")
    println(s"layer = $layerCount")
    println(s"width = $widthFull")
    println(s"dspCost = $dspCost")
    println(s"splitCost = $splitCost")
    println(s"mergeCost = $mergeCost")
    println(s"clbCost = $clbCost")
  }

  override def toString = s"Big Multiplier Solution for ${className(multiplierType)}: " +
    s"\n\t$widthFull = $dsp -> ${splits.zip(isKaras).mkString(" -> ")} " +
    s"\n\tdspCost = $dspCost, clbCost = $clbCost"
}


/** description of a decomposition for multiplication, which supports rectangular n-split Karatsuba
 *
 * @param baseSize       size of sub-multiplier
 * @param split          number of splits
 * @param multiplierType multiplication target
 * @param isKara         use Karatsuba/School book decomposition
 */
case class BmDecomposition(baseSize: (Int, Int), split: Int, multiplierType: MultiplierType, isKara: Boolean = true) {

  /** --------
   * get sizes
   * -------- */

  val (baseHeight, baseWidth) = baseSize
  //  if (!isKara) require(baseHeight == baseWidth)
  val segmentWidth = baseHeight + baseWidth

  // for rectangular situation
  val w = gcd(baseHeight, baseWidth).toInt // common base width

  val factorA = baseHeight / w // factor a
  val factorB = baseWidth / w // factor b
  val sizeBlock = factorA * factorB

  def isRectangular = factorA != factorB

  val aSplit = factorB * split // number of splits, word a
  val bSplit = factorA * split // number of splits, word b

  def widthBlock: Int = sizeBlock * w

  def widthNext: Int = split * factorA * factorB * w

  def widthOut: Int = widthNext * 2

  /** --------
   * costs
   * -------- */
  val karaCount = split * (split - 1) / 2 * sizeBlock

  val diagonalCount = split * sizeBlock

  val schoolbookCount = split * split * sizeBlock

  def subMultCount = multiplierType match {
    case FullMultiplier =>
      if (isKara) karaCount + diagonalCount
      else schoolbookCount
    case _ => karaCount + diagonalCount
  }

  // TODO: vary for different sizes
  val compressorEff = 1.0

  def splitCost: Int =
    if (multiplierType == FullMultiplier && isKara) karaCount * segmentWidth
    else 0

  // TODO: merge cost for MSB/LSB/SQUARE
  def mergeCost: Double = multiplierType match {
    case FullMultiplier => if (isKara) {
      val minusCost = karaCount * 2 * segmentWidth // reduce high & low
      val plusCost = subMultCount * segmentWidth
      val sideCount = karaCount * (segmentWidth + 1) // for tiling
      (minusCost + plusCost + sideCount - widthOut) / compressorEff // reduce = all - remained
    }
    else (subMultCount * segmentWidth - widthOut) / compressorEff
    case SquareMultiplier => subMultCount * segmentWidth - widthOut
    case MsbMultiplier => ???
    case LsbMultiplier => ???
  }

  def clbCost: Double = splitCost + mergeCost

  /** --------
   * operations
   * -------- */
  def typesNext = multiplierType match {
    case FullMultiplier => Seq.fill(subMultCount)(FullMultiplier)
    case SquareMultiplier => Seq.fill(diagonalCount)(SquareMultiplier) ++ Seq.fill(karaCount)(FullMultiplier)
    case MsbMultiplier => ???
    case LsbMultiplier => ???
  }
}

object BmDecomposition {

  def widthNext(baseWidth: (Int, Int), split: Int) =
    BmDecomposition(baseWidth, split, FullMultiplier).widthNext

}
