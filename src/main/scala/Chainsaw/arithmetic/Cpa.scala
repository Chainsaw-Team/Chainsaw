package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._

sealed trait CpaMode

object M2M extends CpaMode

object M2S extends CpaMode

object S2M extends CpaMode

object S2S extends CpaMode

/** carry-propagation adder
 *
 * @param adderType function of adder
 * @param widths    widths of sub-adders, low to high
 * @param cpaMode   interface of adder
 * @param withCarry carry-out existence
 */
case class Cpa(adderType: AdderType, widths: Seq[Int], cpaMode: CpaMode, withCarry: Boolean) extends ChainsawGenerator {

  override def name = s"cpa_${widths.mkString("_")}_${cpaMode.getClass.getSimpleName.init}_${adderType.getClass.getSimpleName.init}_${withCarry}"

  if (widths.exists(_ > 96)) logger.warn(s"way too long single carry chain: ${widths.max}")

  val widthInc = adderType match {
    case BinaryAdder => 1
    case BinarySubtractor => 1
    case TernaryAdder => 2
    case TernarySubtractor1 => 1
    case TernarySubtractor2 => 0
  }

  val widthsWithInc = widths.init :+ (if (withCarry) widths.last + widthInc else widths.last)
  val widthFull = widths.sum

  val operandCount = adderType match {
    case BinaryAdder => 2
    case BinarySubtractor => 2
    case _ => 3
  }

  override var inputTypes = {
    val temp = cpaMode match {
      case M2M => widths.map(UIntInfo(_))
      case M2S => widths.map(UIntInfo(_))
      case _ => Seq(widthFull).map(UIntInfo(_))
    }
    Seq.fill(operandCount)(temp).flatten
  }

  override var outputTypes = cpaMode match {
    case M2M => widthsWithInc.map(UIntInfo(_))
    case S2M => widthsWithInc.map(UIntInfo(_))
    case _ => Seq(widthsWithInc.sum).map(UIntInfo(_))
  }

  override def impl(dataIn: Seq[Any]) = {

    // get concatenated inputs a, b and optional c
    def concat(bigInts: Seq[BigInt], widths: Seq[Int]): BigInt = {
      val str = bigInts.zip(widths).map { case (int, i) => int.toString(2).padToLeft(i, '0') }.reverse.reduce(_ + _)
      BigInt(str, 2)
    }

    val upper = if (withCarry) Pow2(widthFull + widthInc) else Pow2(widthFull)

    def addWrapAround(value: BigInt): BigInt = if (value >= upper) value - upper else value

    def subWrapAround(value: BigInt): BigInt = {
      if (!withCarry) if (value < 0) value + upper else value
      else value + (upper / 2)
    }

    val data = dataIn.asInstanceOf[Seq[BigInt]]
      .grouped(inputWidths.length / operandCount).toSeq
      .map(concat(_, widths))
    val Seq(a, b) = data
    val c: BigInt = if (data.length > 2) data(2) else BigInt(0)
    require(Seq(a, b, c).forall(_ < upper))

    //
    val ret = adderType match {
      case BinaryAdder => addWrapAround(a + b)
      case BinarySubtractor => subWrapAround(a - b)
      case TernaryAdder => a + b + c
      case TernarySubtractor1 => a + b - c
      case TernarySubtractor2 => a - b - c
    }

    val slices = widthsWithInc.scan(0)(_ + _)
    cpaMode match {
      case S2S => Seq(ret)
      case M2S => Seq(ret)
      case _ => slices.prevAndNext { case (prev, next) => ret.toBitValue()(next - 1 downto prev) }
    }
  }

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  override val inputTimes = Some({
    val temp = cpaMode match {
      case M2M => widths.indices
      case M2S => widths.indices
      case _ => Seq(0)
    }
    Seq.fill(operandCount)(temp).flatten
  })

  override val outputTimes = Some(cpaMode match {
    case M2M => widths.indices
    case S2M => widths.indices
    case _ => Seq(0)
  })

  override var latency = cpaMode match {
    case M2M => 1
    case S2M => 1
    case _ => widths.length
  }

  override def implH = new ChainsawModule(this) {

    val coreCount = widths.length // number of subAdders
    val inputTimesExtended = actualInTimes.padTo(coreCount, 0)
    val outputTimesExtended = actualOutTimes.padTo(coreCount, 0)
    val inputCompensations = widths.indices.zip(inputTimesExtended).map { case (target, actual) => target - actual }
    val outputCompensations = outputTimesExtended.zip(widths.indices).map { case (target, actual) => target + latency - actual }

    val dataWords: Seq[Seq[UInt]] = // a mesh of dataWords where each column contains all words of an operand
      {
        if (cpaMode == S2M | cpaMode == S2S) {
          val slices = widths.scan(0)(_ + _).prevAndNext { case (prev, next) => (next - 1) downto prev }
          uintDataIn.map(operand => slices.map(operand(_)))
        } else uintDataIn.grouped(coreCount).toSeq
      }.transpose

    // prepare output words
    val sumWords = widths.map(w => UInt(w bits)) //

    val carriesStart = adderType match {
      case BinaryAdder => Seq(False)
      case BinarySubtractor => Seq(True)
      case TernaryAdder => Seq(False, False)
      case TernarySubtractor1 => Seq(False, True)
      case TernarySubtractor2 => ???
    }

    // carry chain connection & final carryOut generation
    val finalCarryOut = Seq.iterate((carriesStart, 0), coreCount + 1) { case (carries, i) =>
      adderType match {
        // TODO: use primitive for binary modes
        case BinaryAdder =>
          val cin = carries.head
          val Seq(x, y) = dataWords(i).map(_.d(inputCompensations(i)))
          val ret = x +^ y + cin.asUInt
          sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
          (Seq(ret.msb.d(1)), i + 1)
        case BinarySubtractor =>
          val cin = carries.head
          val Seq(x, y) = dataWords(i).map(_.d(inputCompensations(i)))
          val ret = x +^ ~y + cin.asUInt
          sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
          (Seq(ret.msb.d(1)), i + 1)
        // TODO: implementations for ternary modes
        case _ => ???
      }
    }.last._1

    val lastWord = if (withCarry) finalCarryOut.head.asUInt @@ sumWords.last else sumWords.last

    val outputWords = (sumWords.init :+ lastWord).map(_.asBits)

    if (cpaMode == M2S | cpaMode == S2S) dataOut.head := outputWords.reverse.reduce(_ ## _)
    else dataOut.zip(outputWords).foreach { case (port, bits) => port := bits } // low to high

    Seq.tabulate(operandCount, inputWidths.length / operandCount)((i, j) => dataIn(i * inputWidths.length / operandCount + j).setName(s"operand_${i}_$j"))

    def isNegative = ~uintDataOut.head.msb
  }
}

object CpaS2S {
  def apply(adderType: AdderType, width: Int, withCarry: Boolean) =
    Cpa(adderType, getBinaryWidths(width), S2S, withCarry)
}
