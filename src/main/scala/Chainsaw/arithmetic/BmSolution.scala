package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx.VivadoUtil
import breeze.numerics.ceil

// TODO: include Bcm as part of the search space
// TODO: include embedded DSPs of expanded size

/** blueprint to build a big multiplier by divide-and-conquer method
  *
  * @param baseMultiplier
  *   size and cost of embedded multiplier
  * @param splits
  *   number of splits for each layer, bottom-up
  * @param multiplierType
  *   multiplication target
  * @param isKaras
  *   use Karatsuba/School book BmDecomposition for each layer, bottom-up
  * @param constant
  *   Some(constant) if the multiplication is constant, None otherwise
  * @param threshold
  *   weight for a multiplication to use a dsp
  */
case class BmSolution(
    baseMultiplier: MultAttribute, // TODO: provide a series of base multipliers, including FULL/MSB/LSB
    splits: Seq[Int],
    multiplierType: MultiplierType,
    isKaras: Seq[Boolean],
    constant: Option[BigInt] = None,
    threshold: Int           = 8
) extends MultAttribute {

  require(splits.length == isKaras.length)

  val dspSize         = (baseMultiplier.widthX, baseMultiplier.widthY)
  val length          = splits.length
  val isEmpty         = length == 0
  val layerCount: Int = splits.length

  /** baseWidth of each layer in bottom-up order
    */
  val allBaseWidths: Seq[(Int, Int)] = Seq
    .iterate((dspSize, 0), layerCount) { case (width, i) =>
      val widthNext = BmDecomposition.widthNext(width, splits(i))
      ((widthNext, widthNext), i + 1)
    }
    .map(_._1)

  def topDecomposition = BmDecomposition(
    allBaseWidths.last,
    splits.last,
    multiplierType,
    isKaras.last
  )

  def widthFull: Int = if (isEmpty) {
    require(dspSize._1 == dspSize._2)
    dspSize._1
  } else topDecomposition.widthNext

  def widthOut: Int = widthFull * 2

  override def widthX = widthFull

  override def widthY = widthFull

  lazy val algo = new BmAlgo(this)

  override def vivadoUtilEstimation = algo.vivadoUtilEstimation

  def vivadoUtil = VivadoUtil(lut = ceil(clbCost).toInt, dsp = dspCost)

  /** -------- operations
    * --------
    */

  def expand(split: Int, isKara: Boolean) = BmSolution(
    baseMultiplier,
    splits :+ split,
    multiplierType,
    isKaras :+ isKara,
    threshold = threshold
  )

  def subSolution(typeSub: MultiplierType) =
    BmSolution(baseMultiplier, splits.init, typeSub, isKaras.init)

  override def toString = {
    val baseString: String = s"${dspSize._1} X ${dspSize._2} base multiplier"
    val splitStrings: Seq[String] = splits.zip(isKaras).map { case (n, bool) =>
      s"$n-split ${if (bool) "Karatsuba" else "Schoolbook"}"
    }
    s"Divide-and-conquer Solution for $widthFull-bit ${className(multiplierType)}: " +
      s"\n\t${(baseString +: splitStrings).mkString(" -> ")}" +
      s"\n\tdspCost = $dspCost, clbCost = $clbCost"
  }
}

/** description of a decomposition for multiplication, which supports
  * rectangular n-split Karatsuba
  *
  * @param baseSize
  *   size of sub-multiplier
  * @param split
  *   number of splits
  * @param multiplierType
  *   multiplication target
  * @param isKara
  *   use Karatsuba/School book decomposition
  */
case class BmDecomposition(
    baseSize: (Int, Int),
    split: Int,
    multiplierType: MultiplierType,
    isKara: Boolean = true
) {

  /** -------- get sizes
    * --------
    */
  val (baseHeight, baseWidth) = baseSize
  //  if (!isKara) require(baseHeight == baseWidth)
  val segmentWidth = baseHeight + baseWidth

  // for rectangular situation
  val w = gcd(baseHeight, baseWidth).toInt // common base width

  val factorA   = baseHeight / w // factor a
  val factorB   = baseWidth / w  // factor b
  val sizeBlock = factorA * factorB

  def isRectangular = factorA != factorB

  val aSplit = factorB * split // number of splits, word a
  val bSplit = factorA * split // number of splits, word b

  def widthBlock: Int = sizeBlock * w

  def widthNext: Int = split * factorA * factorB * w

  def widthOut: Int = widthNext * 2
}

object BmDecomposition {
  def widthNext(baseWidth: (Int, Int), split: Int) =
    BmDecomposition(baseWidth, split, FullMultiplier).widthNext
}
