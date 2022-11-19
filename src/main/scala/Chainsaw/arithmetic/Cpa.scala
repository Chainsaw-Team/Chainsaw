package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

sealed trait CpaMode

object M2M extends CpaMode

object M2S extends CpaMode

object S2M extends CpaMode

object S2S extends CpaMode

/** carry-propagation adder
  *
  * @param adderType
  *   function of adder
  * @param widths
  *   widths of sub-adders, low to high
  * @param cpaMode
  *   interface of adder
  * @param withCarry
  *   carry-out existence
  */
case class Cpa(adderType: AdderType, widths: Seq[Int], cpaMode: CpaMode, withCarry: Boolean) extends ChainsawGenerator {

  override def name = s"cpa_${widths.mkString("_")}_${cpaMode.getClass.getSimpleName.init}_${adderType.getClass.getSimpleName.init}_$withCarry"

  if (widths.exists(_ > cpaWidthMax)) logger.warn(s"way too long single carry chain: ${widths.max}")

  val widthInc = adderType match {
    case BinaryAdder => 1
    // when you want the sign of subtraction, set 1 bit more on width, then MSB = 1 means negative
    case BinarySubtractor   => 0
    case TernaryAdder       => 2
    case TernarySubtractor1 => 1
    case TernarySubtractor2 => 0
  }

  val widthsWithInc = widths.init :+ (if (withCarry) widths.last + widthInc else widths.last)
  val widthFull     = widths.sum

  val operandCount = adderType match {
    case BinaryAdder      => 2
    case BinarySubtractor => 2
    case _                => 3
  }

  override var inputTypes = {
    val temp = cpaMode match {
      case M2M => widths.map(UIntInfo(_))
      case M2S => widths.map(UIntInfo(_))
      case _   => Seq(widthFull).map(UIntInfo(_))
    }
    Seq.fill(operandCount)(temp).flatten
  }

  override var outputTypes = cpaMode match {
    case M2M => widthsWithInc.map(UIntInfo(_))
    case S2M => widthsWithInc.map(UIntInfo(_))
    case _   => Seq(widthsWithInc.sum).map(UIntInfo(_))
  }

  override def impl(dataIn: Seq[Any]) = {

    // get concatenated inputs a, b and optional c
    def concat(bigInts: Seq[BigInt], widths: Seq[Int]): BigInt = {
      val str = bigInts.zip(widths).map { case (int, i) => int.toString(2).padToLeft(i, '0') }.reverse.reduce(_ + _)
      BigInt(str, 2)
    }

    val upper = if (withCarry) Pow2(widthFull + widthInc) else Pow2(widthFull)

    def addWrapAround(value: BigInt): BigInt = if (value >= upper) value % upper else value

    def subWrapAround(value: BigInt): BigInt = {
      if (!withCarry) {
        if (value < 0) { if (value < -upper) value + 2 * upper else value + upper }
        else value % upper
      } else {
        if (value < 0) { if (value < -upper) value + 2 * upper else value + upper }
        else value
      }
    }

    val data = dataIn
      .asInstanceOf[Seq[BigInt]]
      .grouped(inputWidths.length / operandCount)
      .toSeq
      .map(concat(_, widths))

    val Seq(a, b) = data.take(2)
    val c: BigInt = if (data.length > 2) data(2) else BigInt(0)
    require(Seq(a, b, c).forall(_ < upper))

    val ret = adderType match {
      case BinaryAdder        => addWrapAround(a + b)
      case BinarySubtractor   => subWrapAround(a - b)
      case TernaryAdder       => addWrapAround(a + b + c)
      case TernarySubtractor1 => subWrapAround(a + b - c)
      case TernarySubtractor2 => subWrapAround(a - b - c)
    }

    val slices = widthsWithInc.scan(0)(_ + _)
    cpaMode match {
      case S2S => Seq(ret)
      case M2S => Seq(ret)
      case _   => slices.prevAndNext { case (prev, next) => ret.toBitValue()(next - 1 downto prev) }
    }
  }

  override var inputFormat  = inputNoControl
  override var outputFormat = outputNoControl

  override val inputTimes = Some({
    val temp = cpaMode match {
      case M2M => widths.indices
      case M2S => widths.indices
      case _   => Seq(0)
    }
    Seq.fill(operandCount)(temp).flatten
  })

  override val outputTimes = Some(cpaMode match {
    case M2M => widths.indices
    case S2M => widths.indices
    case _   => Seq(0)
  })

  override var latency = cpaMode match {
    case M2M => 1
    case S2M => 1
    case _   => widths.length
  }

  val coreCount           = widths.length // number of subAdders
  val inputTimesExtended  = actualInTimes.padTo(coreCount, 0)
  val outputTimesExtended = actualOutTimes.padTo(coreCount, 0)
  val inputCompensations  = widths.indices.zip(inputTimesExtended).map { case (target, actual) => target - actual }
  val outputCompensations = outputTimesExtended.zip(widths.indices).map { case (target, actual) => target + latency - actual }

  override def implH: ChainsawModule = new ChainsawModule(this) {

    logger.info(s"implementing CPA, width = $widthFull, latency = $latency")

    val dataWords: Seq[Seq[UInt]] = { // a mesh of dataWords where each column contains all words of an operand
      {
        if (cpaMode == S2M | cpaMode == S2S) {
          val slices = widths.scan(0)(_ + _).prevAndNext { case (prev, next) => (next - 1) downto prev }
          uintDataIn.map(operand => slices.map(operand(_)))
        } else uintDataIn.grouped(coreCount).toSeq
      }.transpose
    }
    // prepare output words
    val sumWords = widths.map(w => UInt(w bits))

    val carriesStart = adderType match {
      case BinaryAdder        => Seq(False)
      case BinarySubtractor   => Seq(True)
      case TernaryAdder       => Seq(False, False)
      case TernarySubtractor1 => Seq(False, True)
      case TernarySubtractor2 => Seq(True, True)
    }

    // carry chain connection & final carryOut generation
    val finalCarryOut = Seq
      .iterate((carriesStart, 0), coreCount + 1) { case (carries, i) =>
        adderType match {
          case BinaryAdder =>
            val cin       = carries.head
            val Seq(x, y) = dataWords(i).map(_.d(inputCompensations(i)))
            val core      = Compressor3to1(x.getBitsWidth)
            core.cIn0   := False
            core.cIn1   := cin
            core.x      := x
            core.y      := y
            core.z      := U(0)
            sumWords(i) := core.sumsOut.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
            (Seq((core.cOut0 ^ core.cOut1).d()), i + 1)
          case BinarySubtractor =>
            val cin       = carries.head
            val Seq(x, y) = dataWords(i).map(_.d(inputCompensations(i)))
            val core      = Compressor3to1(x.getBitsWidth, 1)
            core.cIn0   := False
            core.cIn1   := cin
            core.x      := x
            core.y      := U(0)
            core.z      := y
            sumWords(i) := core.sumsOut.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
            (Seq((core.cOut0 ^ core.cOut1).d()), i + 1)
          case TernaryAdder =>
            val Seq(cin1, cin0) = carries
            val Seq(x, y, z)    = dataWords(i).map(_.d(inputCompensations(i)))
            val core            = Compressor3to1(x.getBitsWidth)
            core.cIn0   := cin0
            core.cIn1   := cin1
            core.x      := x
            core.y      := y
            core.z      := z
            sumWords(i) := core.sumsOut.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
            (Seq(core.cOut1.d(), core.cOut0.d()), i + 1)
          case TernarySubtractor1 =>
            val Seq(cin1, cin0) = carries
            val Seq(x, y, z)    = dataWords(i).map(_.d(inputCompensations(i)))
            val core            = Compressor3to1(x.getBitsWidth, 1)
            core.cIn0   := cin0
            core.cIn1   := cin1
            core.x      := x
            core.y      := y
            core.z      := z
            sumWords(i) := core.sumsOut.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
            (Seq(core.cOut1.d(), core.cOut0.d()), i + 1)
          case TernarySubtractor2 =>
            val Seq(cin1, cin0) = carries
            val Seq(x, y, z)    = dataWords(i).map(_.d(inputCompensations(i)))
            val core            = Compressor3to1(x.getBitsWidth, 2)
            core.cIn0   := cin0
            core.cIn1   := cin1
            core.x      := x
            core.y      := y
            core.z      := z
            sumWords(i) := core.sumsOut.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
            (Seq(core.cOut1.d(), core.cOut0.d()), i + 1)
        }
      }
      .last
      ._1

    def isNegative: Bool = adderType match {
      case BinarySubtractor   => ~finalCarryOut.head
      case TernarySubtractor1 => ~finalCarryOut.reduce(_ | _)
      case TernarySubtractor2 => ~finalCarryOut.reduce(_ & _)
      case _                  => False
    }

    def isOverFlow: Bool = adderType match {
      case BinaryAdder        => if (withCarry) False else finalCarryOut.head
      case TernaryAdder       => if (withCarry) False else finalCarryOut.reduce(_ | _)
      case TernarySubtractor1 => if (withCarry) False else finalCarryOut.reduce(_ & _)
      case TernarySubtractor2 => if (withCarry) False else ~finalCarryOut.reduce(_ | _)
      case _                  => False
    }

    val lastWord = adderType match {
      case BinaryAdder        => if (withCarry) finalCarryOut.head.asUInt @@ sumWords.last else sumWords.last
      case BinarySubtractor   => sumWords.last
      case TernaryAdder       => if (withCarry) finalCarryOut.map(_.asUInt).reduce(_ +^ _) @@ sumWords.last else sumWords.last
      case TernarySubtractor1 => if (withCarry) ~(finalCarryOut.head ^ finalCarryOut.last).asUInt @@ sumWords.last else sumWords.last
      case TernarySubtractor2 => sumWords.last
    }
    val outputWords = (sumWords.init :+ lastWord).map(_.asBits)

    if (cpaMode == M2S | cpaMode == S2S) dataOut.head := outputWords.reverse.reduce(_ ## _)
    else dataOut.zip(outputWords).foreach { case (port, bits) => port := bits } // low to high

    Seq.tabulate(operandCount, inputWidths.length / operandCount)((i, j) => dataIn(i * inputWidths.length / operandCount + j).setName(s"operand_${i}_$j"))
  }

  // TODO: impl
  override def implNaiveH = Some(new ChainsawModule(this) {
    // a mesh of dataWords where each column contains all words of an operand
    val dataWords: Seq[Seq[UInt]] = {
      if (cpaMode == S2M | cpaMode == S2S) {
        val slices = widths.scan(0)(_ + _).prevAndNext { case (prev, next) => (next - 1) downto prev }
        uintDataIn.map(operand => slices.map(operand(_)))
      } else uintDataIn.grouped(coreCount).toSeq
    }.transpose
    // prepare output words
    val sumWords = widths.map(w => UInt(w bits))

    // carry chain connection & final carryOut generation
    val (finalCarryOut, negative, _) = Seq
      .iterate((Seq.fill(2)(False), False, 0), coreCount + 1) { case (carries, sub, i) =>
        adderType match {
          case BinaryAdder =>
            val Seq(x, y) = dataWords(i).map(_.d(inputCompensations(i)))
            val ret       = x +^ y + carries.head.asUInt
            sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
            (Seq(ret.msb.d(), False), False, i + 1)
          case BinarySubtractor =>
            val Seq(x, y) = dataWords(i).map(_.d(inputCompensations(i)))
            val ret       = UInt()
            ret := x.expand - sub.asUInt - y.expand
            val isNegative = ret.msb
            val retAbs     = ~ret.takeLow(x.getBitsWidth).asUInt + U(1)
            when(isNegative) { sumWords(i) := ((U(1) << x.getBitsWidth) - retAbs).resize(x.getBitsWidth).d(outputCompensations(i)) }
              .otherwise { sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i)) }
            (Seq.fill(2)(False), isNegative.d(), i + 1)
          case TernaryAdder =>
            val Seq(x, y, z) = dataWords(i).map(_.d(inputCompensations(i)))
            val ret          = x +^ y +^ z + carries.reverse.asBits().asUInt
            sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
            (Seq(ret(x.getBitsWidth + 1).d(), ret(x.getBitsWidth).d()), False, i + 1)
          case TernarySubtractor1 =>
            val Seq(x, y, z) = dataWords(i).map(_.d(inputCompensations(i)))
            val ret          = UInt()
            when(sub) { ret := x.expand +^ y.expand - sub.asUInt - z.expand }
              .otherwise { ret := x.expand +^ y.expand + carries.head.asUInt - z.expand }
            val isNegative = ret.msb
            val retAbs     = ~ret.takeLow(x.getBitsWidth).asUInt + U(1)
            when(isNegative) { sumWords(i) := ((U(1) << x.getBitsWidth) - retAbs).resize(x.getBitsWidth).d(outputCompensations(i)) }
              .otherwise { sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i)) }
            (Seq(ret(x.getBitsWidth).d(), False), isNegative.d(), i + 1)
          case TernarySubtractor2 =>
            val Seq(x, y, z) = dataWords(i).map(_.d(inputCompensations(i)))
            val ret          = UInt()
            when(sub) { ret := x.resize(x.getBitsWidth + 2) - sub.asUInt - carries.head.asUInt - y.resize(y.getBitsWidth + 2) - z.resize(z.getBitsWidth + 2) }
              .otherwise { ret := x.resize(x.getBitsWidth + 2) - y.resize(y.getBitsWidth + 2) - z.resize(z.getBitsWidth + 2) }
            val isNegative = ret.msb
            val downFlow   = !ret(x.getBitsWidth)
            sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
            (Seq.fill(2)(downFlow.d()), isNegative.d(), i + 1)
        }
      }
      .last

    //    def isNegative = ~uintDataOut.head.msb

    def isNegative = adderType match {
      case BinarySubtractor   => negative
      case TernarySubtractor1 => negative
      case TernarySubtractor2 => negative
      case _                  => False
    }

    def isOverFlow = adderType match {
      case BinaryAdder        => if (withCarry) False else finalCarryOut.head
      case TernaryAdder       => if (withCarry) False else finalCarryOut.reduce(_ | _)
      case TernarySubtractor1 => if (withCarry) False else finalCarryOut.head
      case TernarySubtractor2 => if (withCarry) False else finalCarryOut.head
      case _                  => False
    }

    val lastWord = adderType match {
      case BinaryAdder        => if (withCarry) finalCarryOut.head.asUInt @@ sumWords.last else sumWords.last
      case BinarySubtractor   => sumWords.last
      case TernaryAdder       => if (withCarry) finalCarryOut.reverse.asBits().asUInt @@ sumWords.last else sumWords.last
      case TernarySubtractor1 => if (withCarry) finalCarryOut.head.asUInt @@ sumWords.last else sumWords.last
      case TernarySubtractor2 => sumWords.last
    }

    val outputWords = (sumWords.init :+ lastWord).map(_.asBits)

    if (cpaMode == M2S | cpaMode == S2S) uintDataOut.head := outputWords.reverse.reduce(_ ## _).asUInt
    else uintDataOut.zip(outputWords).foreach { case (port, bits) => port.assignFromBits(bits) } // low to high

    Seq.tabulate(operandCount, inputWidths.length / operandCount)((i, j) => uintDataIn(i * inputWidths.length / operandCount + j).setName(s"operand_${i}_$j"))
  })
}

object CpaS2S {
  def apply(adderType: AdderType, width: Int, withCarry: Boolean) =
    Cpa(adderType, getBinaryWidths(width), S2S, withCarry)
}
