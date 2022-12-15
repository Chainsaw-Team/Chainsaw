package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

trait Compressor {
  /** --------
   * definition
   * -------- */

  def inputFormat: Seq[Int]

  def outputFormat: Seq[Int]

  def compress(bitsIn: BitHeapHard): BitHeapHard

  /** --------
   * attributes
   * -------- */
  def inputBitsCount = inputFormat.sum

  def outputBitsCount = outputFormat.sum

  def inputMax = inputFormat.zipWithIndex.map { case (w, i) => w * (BigInt(1) << i) }.sum

  def outputMax = outputFormat.zipWithIndex.map { case (w, i) => w * (BigInt(1) << i) }.sum

  def compact = inputMax == outputMax

  def redundant = !compact

  def bitReduction: Int = inputBitsCount - outputBitsCount

  def heightReduction: Int = inputFormat.max - outputFormat.max

  def reductionRatio: Double = inputBitsCount.toDouble / outputBitsCount

  def vivadoUtilEstimation: VivadoUtil

  def fmaxEstimation: HertzNumber = 600 MHz // target for all compressors

  def latency = 0
}

