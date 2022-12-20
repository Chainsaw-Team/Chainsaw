package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

//case class Cpa(adderType: AdderType, width: Int) extends UnsignedMerge {
//
//  override val inputTimes = inputTypes.map(_ => 0)
//  override val outputTimes = Seq(0)
//
//  override val arithInfos = {
//    val signs = adderType match {
//      case BinaryAdder => Seq(true, true)
//      case BinarySubtractor => Seq(true, false)
//      case TernaryAdder => Seq(true, true, true)
//      case TernarySubtractor1 => Seq(true, true, false)
//      case TernarySubtractor2 => Seq(true, false, false)
//    }
//    signs.map(sign => ArithInfo(width, 0, sign))
//  }
//
//  override def implH = ???
//
//  override def latency() = width.divideAndCeil(cpaWidthMax)
//
//  override def name = s"${className(adderType)}_$width"
//
//  override def vivadoUtilEstimation = VivadoUtilEstimation(lut = width + 2, carry8 = (width + 2).divideAndCeil(8))
//
//  override def fmaxEstimation = 600 MHz
//}

case class Cpa(adderType: AdderType, width: Int) extends UnsignedMerge {

  val coreCount = width.divideAndCeil(cpaWidthMax)

  val slices = Seq.fill(width)(1).grouped(cpaWidthMax).toSeq.map(_.sum).scan(0)(_ + _).prevAndNext { case (prev, next) => (next - 1) downto prev }

  val sumWords = Seq.fill(width)(1).grouped(cpaWidthMax).toSeq.map(_.sum).map(w => UInt(w bits))

  override val arithInfos = {
    val signs = adderType match {
      case BinaryAdder        => Seq(true, true)
      case BinarySubtractor   => Seq(true, false)
      case TernaryAdder       => Seq(true, true, true)
      case TernarySubtractor1 => Seq(true, true, false)
      case TernarySubtractor2 => Seq(true, false, false)
    }
    signs
      .map { sign =>
        (1 to coreCount).map(i => ArithInfo(cpaWidthMax min (width - (i - 1) * cpaWidthMax), 0, sign, i - 1))
      }
      .transpose
      .flatten
  }

  override val inputTimes  = arithInfos.map(_.time)
  override val outputTimes = inputTimes.map(_ + 1)

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(this) {
    val carriesStart = adderType match {
      case BinaryAdder        => Seq(False)
      case BinarySubtractor   => Seq(True)
      case TernaryAdder       => Seq(False, False)
      case TernarySubtractor1 => Seq(False, True)
      case TernarySubtractor2 => Seq(True, True)
    }

    val dataWords = dataIn.map(operand => slices.map(operand.asUInt()(_))).transpose

    val finalCarryOut = Seq
      .iterate((carriesStart, 0), coreCount + 1) { case (carries, i) =>
        adderType match {
          case BinaryAdder =>
            val cin       = carries.head
            val Seq(x, y) = dataWords(i)
            val core      = Compressor3to1(x.getBitsWidth).implH
            core.dataIn := Vec(x, y, U(0)).map(_.toAFix)
            sumWords(i) := core.dataOut.head.asBits.takeLow(x.getBitsWidth).asUInt.d(1)
            (Seq((core.dataOut.head.asBits.msb ^ core.dataOut.last.asBits.msb).d()), i + 1)
          case BinarySubtractor =>
            val cin       = carries.head
            val Seq(x, y) = dataWords(i)
            val core      = Compressor3to1(x.getBitsWidth, 1).implH
            core.dataIn := Vec(x, y, U(0)).map(_.toAFix)
            sumWords(i) := core.dataOut.head.asBits.takeLow(x.getBitsWidth).asUInt.d(1)
            (Seq((core.dataOut.head.asBits.msb ^ core.dataOut.last.asBits.msb).d()), i + 1)
          case TernaryAdder =>
            val Seq(cin1, cin0) = carries
            val Seq(x, y, z)    = dataWords(i)
            val core            = Compressor3to1(x.getBitsWidth).implH
            core.dataIn := Vec(x, y, z).map(_.toAFix)
            sumWords(i) := core.dataOut.head.asBits.takeLow(x.getBitsWidth).asUInt.d(1)
            (Seq(core.dataOut.last.asBits.msb.d(), core.dataOut.head.asBits.msb.d()), i + 1)
          case TernarySubtractor1 =>
            val Seq(cin1, cin0) = carries
            val Seq(x, y, z)    = dataWords(i)
            val core            = Compressor3to1(x.getBitsWidth, 1).implH
            core.dataIn := Vec(x, y, z).map(_.toAFix)
            sumWords(i) := core.dataOut.head.asBits.takeLow(x.getBitsWidth).asUInt.d(1)
            (Seq(core.dataOut.last.asBits.msb.d(), core.dataOut.head.asBits.msb.d()), i + 1)
          case TernarySubtractor2 =>
            val Seq(cin1, cin0) = carries
            val Seq(x, y, z)    = dataWords(i)
            val core            = Compressor3to1(x.getBitsWidth, 2).implH
            core.dataIn := Vec(x, y, z).map(_.toAFix)
            sumWords(i) := core.dataOut.head.asBits.takeLow(x.getBitsWidth).asUInt.d(1)
            (Seq(core.dataOut.last.asBits.msb.d(), core.dataOut.head.asBits.msb.d()), i + 1)
        }
      }
      .last
      ._1

    val lastWord = adderType match {
      case BinaryAdder        => finalCarryOut.head.asUInt @@ sumWords.last
      case BinarySubtractor   => sumWords.last
      case TernaryAdder       => finalCarryOut.map(_.asUInt).reduce(_ +^ _) @@ sumWords.last
      case TernarySubtractor1 => ~(finalCarryOut.head ^ finalCarryOut.last).asUInt @@ sumWords.last
      case TernarySubtractor2 => sumWords.last
    }
    val outputWords = (sumWords.init :+ lastWord).map(_.asBits)

    dataOut := Vec(outputWords.reverse.reduce(_ ## _).asUInt.toAFix)
  }

  override def latency() = coreCount

  override def name = s"${className(adderType)}_$width"

  override def vivadoUtilEstimation = VivadoUtilEstimation(lut = width + 2, carry8 = (width + 2).divideAndCeil(8))

  override def fmaxEstimation = 600 MHz

}
