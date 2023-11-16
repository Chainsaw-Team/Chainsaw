package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.arithmetic.bitheap.ScoreIndicator
import Chainsaw.edaFlow.vivado._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import bitheap._

import scala.collection.immutable
import scala.util.Random

trait Compressor {

  /** -------- definition --------
    */
  var isPipeline = false

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

  def clbCost = if (vivadoUtilEstimation.clbCost(isPipeline) == 0.0) 0.5 else vivadoUtilEstimation.clbCost(isPipeline)

  def bitReduction: Int = inputBitsCount - outputBitsCount

  def heightReduction: Int = inputFormat.max - outputFormat.max

  def reductionRatio: Double = inputBitsCount.toDouble / outputBitsCount

  def reductionEfficiency: Double = bitReduction / clbCost

  def compressorScores =
    ScoreIndicator(
      bitReduction,
      heightReduction,
      reductionEfficiency,
      reductionRatio,
      clbCost
    )

  // hardware requirement
  def fmaxEstimation: HertzNumber = 600 MHz // target for all compressors

  def latency = 0

  // set pipeline state
  def setPipelineState(pipeline: Boolean) = isPipeline = pipeline

  def resetPipelineState() = isPipeline = false

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

trait CompressorGenerator extends ChainsawOperatorGenerator with Compressor with FixedLatency {

  override def latency: Int = 0

  // complement related
  def complementHeap: Seq[Seq[Boolean]]

  /** this method is used to get the complement configuration information of this [[CompressorGenerator]]
    * @return
    *   the complement information of this [[CompressorGenerator]]
    */
  def getComplementHeap: Seq[Seq[Boolean]] =
    if (complementHeap == null) inputFormat.map(i => Seq.fill(i)(true))
    else complementHeap

  /** this method is used to indicate whether this [[CompressorGenerator]] is running on complement pattern
    * @return
    *   the Boolean which indicate whether this [[CompressorGenerator]] is running on complement pattern
    */
  def shouldDoComplement: Boolean =
    !getComplementHeap.forall(_.forall(_ == getComplementHeap.head.head))

  // for verification
  /** this method is used to generate a random complement configuration information of this [[CompressorGenerator]]
    * @return
    *   a random complement information of this [[CompressorGenerator]]
    */
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

  /** this method is used to compress a [[BitHeapHard]] to another [[BitHeapHard]] using this [[CompressorGenerator]]
    * @param bitsIn
    *   the input [[BitHeapHard]] of this [[CompressorGenerator]]
    * @return
    *   the output [[BitHeapHard]] of this [[CompressorGenerator]]
    */
  def compress(bitsIn: BitHeapHard): BitHeapHard = {
    require(bitsIn.zip(complementHeap).forall { case (buffer, booleans) =>
      buffer.zip(booleans).forall { case (bit, bool) =>
        bit.notComplement == bool
      }
    })
    this match {
      case _: Compressor1to1 => bitsIn
      case _ =>
        val paddedBitsIn = bitsIn
          .zip(inputFormat)
          .map { case (bits, h) =>
            bits.padTo(h, Bit(False))
          }
          .filter(_.nonEmpty)
        val operands = this match {
          case _: Gpc =>
            paddedBitsIn.map(_.map(_.value)).map(_.asBits().asUInt.toAFix)
          case _: RowAdder => columns2Operands(paddedBitsIn)
        }
        val core = getImplH
        core.dataIn := operands
        operands2Columns(core.dataOut, outputFormat, complement = false)
//        val complementOut = bitsIn.forall(_.forall(!_.notComplement)) // only if all bits are complement, the output is complement
//        operands2Columns(core.dataOut, outputFormat, complementOut)
    }
  }

  /** this method is used to compress a HardType [[BitHeap]] to another HardType [[BitHeap]] using this
    * [[CompressorGenerator]]
    * @param bitsIn
    *   the input HardType [[BitHeap]] of this [[CompressorGenerator]]
    * @return
    *   the output HardType [[BitHeap]] of this [[CompressorGenerator]]
    */
  def compressHard(bitsIn: BitHeap[Bool]): BitHeap[Bool] = {
    bitsIn.heap.zip(inputFormat).foreach { case (col, h) =>
      col.padTo(h, False)
    }
    val retHeap = BitHeap(compress(bitsIn.heap), bitsIn.weightLow, bitsIn.time)
    retHeap.addConstant(bitsIn.constant)
    retHeap
  }

  /** this method is used to compress a SoftType [[BitHeap]] to another SoftType [[BitHeap]] using this
    * [[CompressorGenerator]]
    * @param bitsIn
    *   the input SoftType [[BitHeap]] of this [[CompressorGenerator]]
    * @return
    *   the output SoftType [[BitHeap]] of this [[CompressorGenerator]]
    */
  def compressSoft(bitsIn: BitHeap[BigInt]): BitHeap[BigInt] = {
    val heapOut =
      BitHeap.fromHeights(outputFormat, bitsIn.weightLow, bitsIn.time)
    heapOut.allocate(bitsIn.evalBigInt)
    heapOut
  }

  /** this method is used to convert the columns of a [[BitHeap]] to the sequence of [[ArithInfo]] format
    * @param columns
    *   the columns of a [[BitHeap]]
    * @return
    *   the sequence of [[ArithInfo]] format of this [[BitHeap]]
    */
  def columns2Infos(columns: Seq[Int]): Seq[ArithInfo] = {
    (0 until columns.max).map(i =>
      ArithInfo(
        width  = columns.count(_ > i),
        weight = columns.indexWhere(_ > i)
      )
    )
  }

  /** this method is used to convert the columns of a HardType [[BitHeap]] to the sequence of operands using [[AFix]]
    * format
    * @param columns
    *   the columns of a HardType [[BitHeap]]
    * @return
    *   the sequence of operands using [[AFix]] format of this [[BitHeap]]
    */
  def columns2Operands(columns: Seq[Seq[Bit[Bool]]]): Seq[AFix] = {
    val intColumns = columns.map(col => col.map(_ => 1).sum)
    (0 until intColumns.max).map { i =>
      (columns
        .filter(_.length > i)
        .map(col => col(i).value)
        .asBits()
        .asUInt << intColumns.indexWhere(_ > i)).toAFix
    }
  }

  /** this method is used to convert the operands to the [[BitHeapHard]] format for generating HardType [[BitHeap]]
    * @param operands
    *   the operands which will be convert to the rows of [[BitHeap]]
    * @param operandsFormat
    *   the operands format for generating [[BitHeapHard]], it describe the columns information of this HardType
    *   [[BitHeap]]
    * @param complement
    *   describe whether all bits in this [[BitHeap]] need complement
    * @return
    *   the [[BitHeapHard]] format according to configuration information for generating HardType [[BitHeap]]
    */
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
