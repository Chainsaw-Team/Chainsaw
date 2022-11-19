package Chainsaw.arithmetic

import Chainsaw.xilinx.VivadoUtilRequirement
import breeze.numerics.ceil
import Chainsaw._

case class KaratsubaSolution(dsp: (Int, Int), splits: Seq[Int]) {

  // all decompositions
  def head: Decomposition = Decomposition(splits.head, baseHeight = dsp._1, baseWidth = dsp._2)

  def decompositions: Seq[Decomposition] = Seq.iterate((head, 0), splits.length) { case (decomposition, i) => (decomposition * splits(i + 1), i + 1) }
    .map(_._1)

  def layer: Int = splits.length

  def *(split: Int) = KaratsubaSolution(dsp, splits :+ split)

  def widthFull: Int = decompositions.last.widthNext

  def dspCost: Int = decompositions.map(_.subMultCount).product

  def subCounts = decompositions.reverse.inits.toSeq.init // number of sub modules in each layer
    .map(chain => chain.map(_.subMultCount).product).tail :+ 1

  def splitCost = {
    val costs = decompositions.map(_.splitCost)
    subCounts.zip(costs).map { case (count, cost) => count * cost }.sum
  }

  def mergeCost = {
    val costs = decompositions.map(_.mergeCost)
    subCounts.zip(costs).map { case (count, cost) => count * cost }.sum
  }

  def clbCost = splitCost + mergeCost

  def vivadoUtil = VivadoUtilRequirement(lut = ceil(clbCost).toInt, dsp = dspCost)

  /** cost when CLB/DSP cost is specified
   *
   * @param clbPerDsp CLB/DSP ration in your FPGA budget
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

// TODO: cost & count for different Karatsuba Mode - Square, Full, MSB and LSB

/** description of a rectangular n-split Karatsuba
 *
 * @param isKara     use Karatsuba/School book decomposition
 * @param split      number of splits
 * @param baseHeight ''height'' of a basic multiplier
 * @param baseWidth  ''width'' of a basic multiplier
 */
case class Decomposition(split: Int, baseHeight: Int, baseWidth: Int, isKara: Boolean = true) {

  if (!isKara) require(baseHeight == baseWidth)

  val w = gcd(baseHeight, baseWidth).toInt // common base width

  val factorA = baseHeight / w // factor a
  val factorB = baseWidth / w // factor b

  val aSplit = factorB * split // number of splits, word a
  val bSplit = factorA * split // number of splits, word b

  val widthFull = split * factorA * factorB * w
  val widthBlock = factorA * factorB * w

  val compressorEff = 1.0

  def widthNext: Int = split * factorA * factorB * w

  def *(split: Int) = Decomposition(split, widthNext, widthNext)

  /** --------
   * costs
   * -------- */

  val segmentWidth = baseHeight + baseWidth

  def subMultCount =
    if (isKara) split * (split + 1) / 2 * factorA * factorB
    else split * split

  def karaCount =
    if (isKara) split * (split - 1) / 2 * factorA * factorB
    else 0

  def splitCost: Int =
    if (isKara) karaCount * segmentWidth
    else 0

  def mergeCost: Double = {
    if (isKara) {
      val minusCost = karaCount * 2 * segmentWidth // reduce high & low
      val plusCost = subMultCount * segmentWidth
      val sideCount = karaCount * (segmentWidth + 1) // for tiling
      (minusCost + plusCost + sideCount - widthNext) / compressorEff // reduce = all - remained
    }
    else subMultCount * segmentWidth - widthNext
  }

  def clbCost: Double = splitCost + mergeCost
}
