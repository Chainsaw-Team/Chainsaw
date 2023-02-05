package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.arithmetic.bitheap.CompressorScores
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import bitheap._

import scala.util.Random

trait Compressor {

  /** -------- definition --------
    */
  def inputFormat: Seq[Int]

  def outputFormat: Seq[Int]

  def vivadoUtilEstimation: VivadoUtil

  /** -------- attributes --------
    */

  // redundancy
  def inputMax = inputFormat.zipWithIndex.map { case (w, i) =>
    w * (BigInt(1) << i)
  }.sum

  def outputMax = outputFormat.zipWithIndex.map { case (w, i) =>
    w * (BigInt(1) << i)
  }.sum

  def compact = inputMax == outputMax

  def redundant = !compact

  // optimal performance
  def inputBitsCount = inputFormat.sum

  def outputBitsCount = outputFormat.sum

  def clbCost(shouldPipeline: Boolean = true) =
    vivadoUtilEstimation.clbCost(shouldPipeline)

  def bitReduction: Int = inputBitsCount - outputBitsCount

  def heightReduction: Int = inputFormat.max - outputFormat.max

  def reductionRatio: Double = inputBitsCount.toDouble / outputBitsCount

  def reductionEfficiency(considerFF: Boolean = true): Double =
    bitReduction / clbCost(considerFF)

  def compressorScores(considerFF: Boolean) =
    CompressorScores(
      bitReduction,
      heightReduction,
      reductionEfficiency(considerFF),
      reductionRatio
    )

  // hardware requirement
  def fmaxEstimation: HertzNumber = 600 MHz // target for all compressors

  def latency = 0

  override def toString = {
    val title =
      s"(${inputFormat.mkString(",")}) -> (${outputFormat.mkString(",")})"
    val dotsIn    = BitHeap.fromHeights(inputFormat, 0, 0).toString
    val dotsOut   = BitHeap.fromHeights(outputFormat, 0, 0).toString
    val length    = outputFormat.length
    val arrowLine = s"${" " * (length / 2) * 2}$downArrow"
    val shiftedDotsIn =
      dotsIn.split("\n").map(_.padToLeft(length * 2 - 1, ' ')).mkString("\n")
    s"$title\n$shiftedDotsIn\n$arrowLine\n$dotsOut"
  }
}

trait CompressorGenerator
    extends ChainsawOperatorGenerator
    with Compressor
    with FixedLatency {

  override def latency: Int = 0

  // complement related
  def complementHeap: Seq[Seq[Boolean]]

  def getComplementHeap: Seq[Seq[Boolean]] =
    if (complementHeap == null) inputFormat.map(i => Seq.fill(i)(true))
    else complementHeap

  def shouldDoComplement: Boolean =
    !getComplementHeap.forall(_.forall(_ == getComplementHeap.head.head))

  // for verification
  def getRandomComplementHeap: Seq[Seq[Boolean]] =
    inputFormat.map(i => Seq.fill(i)(Random.nextBoolean()))

  // golden model
  override def testCases = Seq.fill(1000)(randomTestCase)

  override def impl(testCase: TestCase) = {
    // operands 2 bits
    val valueHeap = Seq.fill(inputFormat.length)(ArrayBuffer[BigInt]())
    this match {
      case _: Gpc => // columns as operands
        testCase.data
          .map(_.toBigInt())
          .zip(valueHeap)
          .foreach { case (int, col) =>
            int
              .toString(2)
              .reverse
              .foreach(char => col.append(BigInt(char.asDigit)))
          }
        valueHeap.zip(inputFormat).foreach { case (col, i) =>
          col ++= Seq.fill(i - col.length)(BigInt(0))
        } // padding
      case _: RowAdder => // rows as operands
        testCase.data
          .map(_.toBigInt())
          .zip(inputTypes.map(_.bitWidth))
          .foreach { case (bigInt, w) =>
            bigInt
              .toString(2)
              .reverse
              .padTo(w, '0') // padding
              .zip(valueHeap)
              .foreach { case (c, col) => col.append(BigInt(c.asDigit)) }
          }
    }

    val retHeap = valueHeap
      .zip(getComplementHeap)
      .map { case (ints, booleans) =>
        ints
          .zip(booleans)
          .map { case (int, bool) =>
            if (bool) int else BigInt(1) - int
          } // get complement
      }

    val ret = retHeap.zipWithIndex.map { case (col, weight) =>
      col.sum << weight
    }.sum
    Seq(BigDecimal(ret)).padTo(outputTypes.length, BigDecimal(0))
  }

  def compress(bitsIn: BitHeapHard): BitHeapHard = {
    require(bitsIn.zip(complementHeap).forall { case (buffer, booleans) =>
      buffer.zip(booleans).forall { case (bit, bool) =>
        bit.notComplement == bool
      }
    })
    this match {
      case _: Compressor1to1 => bitsIn
      case _ =>
        val paddedBitsIn = bitsIn.zip(inputFormat).map { case (bits, h) =>
          bits.map(_.value).padTo(h, False)
        }
        val operands = this match {
          case _: Gpc      => paddedBitsIn.map(_.asBits().asUInt.toAFix)
          case _: RowAdder => columns2Operands(paddedBitsIn)
        }
        val core = getImplH
        core.dataIn := operands
        operands2Columns(core.dataOut, outputFormat, false)
//        val complementOut = bitsIn.forall(_.forall(!_.notComplement)) // only if all bits are complement, the output is complement
//        operands2Columns(core.dataOut, outputFormat, complementOut)
    }
  }

  def compressHard(bitsIn: BitHeap[Bool]): BitHeap[Bool] = {
    bitsIn.heap.zip(inputFormat).foreach { case (col, h) =>
      col.padTo(h, False)
    }
    val retHeap = compress(bitsIn.heap)
    BitHeap(retHeap, bitsIn.weightLow, bitsIn.time)
  }

  def compressSoft(bitsIn: BitHeap[BigInt]): BitHeap[BigInt] = {
    val heapOut =
      BitHeap.fromHeights(outputFormat, bitsIn.weightLow, bitsIn.time)
    heapOut.allocate(bitsIn.evalBigInt)
    heapOut
  }

  def columns2Infos(columns: Seq[Int]) = {
    (0 until columns.max).map(i =>
      ArithInfo(
        width  = columns.count(_ > i),
        weight = columns.indexWhere(_ > i)
      )
    )
  }

  def columns2Operands(columns: Seq[Seq[Bool]]): Seq[AFix] = {
    val intColumns = columns.map(col => col.map(_ => 1).sum)
    (0 until intColumns.max).map { i =>
      (columns
        .filter(_.length > i)
        .map(col => col(i))
        .asBits()
        .asUInt << intColumns.indexWhere(_ > i)).toAFix
    }
  }

  def operands2Columns(
      operands: Seq[AFix],
      operandsFormat: Seq[Int],
      complement: Boolean
  ): BitHeapHard = {
    val infos   = columns2Infos(operandsFormat)
    val width   = infos.map(info => info.high + 1).max
    val columns = ArrayBuffer.fill(width)(ArrayBuffer[Bit[Bool]]())
    operands.map(_.asBits).zip(infos).foreach { case (bits, info) =>
      val bitWidth = bits.getBitsWidth
      require(
        bitWidth == info.width,
        s"operand width mismatch, operand: $bitWidth, format: ${info.width}"
      )
      (0 until bitWidth).foreach { bw =>
        columns(bw + info.weight) += Bit(bits(bw), !complement)
      }
    }
    columns
  }
}
