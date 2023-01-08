package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

case class Cpa(adderType: AdderType, width: Int) extends UnsignedMerge {

  val coreCount = width.divideAndCeil(cpaWidthMax)

  val sliceWidths = Seq
    .fill(width)(1)
    .grouped(cpaWidthMax)
    .toSeq
    .map(_.sum) // width of each slice
  val slices = sliceWidths.scan(0)(_ + _).prevAndNext { case (prev, next) =>
    (next - 1) downto prev
  } // start and end of each slice

  override def arithInfos = {
    val signs = adderType match {
      case BinaryAdder        => Seq(true, true)
      case BinarySubtractor   => Seq(true, false)
      case TernaryAdder       => Seq(true, true, true)
      case TernarySubtractor1 => Seq(true, true, false)
      case TernarySubtractor2 => Seq(true, false, false)
    }
    signs.map(ArithInfo(width, 0, _))
  }

  override def positiveLength =
    arithInfos.filter(_.isPositive).map(_.maxValue).sum.bitLength

  override def outputTypes = Seq(NumericType.U(positiveLength))

  override def outputArithInfos = Seq(ArithInfo(positiveLength, 0))

  override def implH: ChainsawOperatorModule = new ChainsawOperatorModule(
    this
  ) {
    val sumWords = sliceWidths.map(w => UInt(w bits))
    val carriesStart = adderType match {
      case BinaryAdder        => Seq(False)
      case BinarySubtractor   => Seq(True)
      case TernaryAdder       => Seq(False, False)
      case TernarySubtractor1 => Seq(False, True)
      case TernarySubtractor2 => Seq(True, True)
    }

    val dataWords = dataIn
      .map(operand =>
        slices.zipWithIndex.map { case (slice, i) =>
          operand.asUInt()(slice).d(i)
        }
      )
      .transpose

    val finalCarryOut = Seq
      .iterate((carriesStart, 0), coreCount + 1) {
        case (carries, i) => // let the carries propagate through the iteration
          adderType match {
            // FIXME: binary adder for better performance
            // TODO: reduce duplication
            case BinaryAdder =>
              val cin       = carries.head
              val Seq(x, y) = dataWords(i)
              val core      = Compressor3to1(x.getBitsWidth).implH
              core.dataIn := Vec(x, y, U(0), U(0), cin.asUInt).map(_.toAFix)
              sumWords(i) := core.dataOut.head.asBits
                .takeLow(x.getBitsWidth)
                .asUInt
                .d(coreCount - i)
              (
                Seq(
                  (core.dataOut.head.asBits.msb ^ core.dataOut.last.asBits.msb)
                    .d()
                ),
                i + 1
              )
            case BinarySubtractor =>
              val cin       = carries.head
              val Seq(x, y) = dataWords(i)
              val core      = Compressor3to1(x.getBitsWidth, 1).implH
              core.dataIn := Vec(x, U(0), y, U(0), cin.asUInt).map(_.toAFix)
              sumWords(i) := core.dataOut.head.asBits
                .takeLow(x.getBitsWidth)
                .asUInt
                .d(coreCount - i)
              (
                Seq(
                  (core.dataOut.head.asBits.msb ^ core.dataOut.last.asBits.msb)
                    .d()
                ),
                i + 1
              )
            case TernaryAdder =>
              val Seq(cin1, cin0) = carries
              val Seq(x, y, z)    = dataWords(i)
              val core            = Compressor3to1(x.getBitsWidth).implH
              core.dataIn := Vec(x, y, z, cin0.asUInt, cin1.asUInt).map(
                _.toAFix
              )
              sumWords(i) := core.dataOut.head.asBits
                .takeLow(x.getBitsWidth)
                .asUInt
                .d(coreCount - i)
              (
                Seq(
                  core.dataOut.last.asBits.msb.d(),
                  core.dataOut.head.asBits.msb.d()
                ),
                i + 1
              )
            case TernarySubtractor1 =>
              val Seq(cin1, cin0) = carries
              val Seq(x, y, z)    = dataWords(i)
              val core            = Compressor3to1(x.getBitsWidth, 1).implH
              core.dataIn := Vec(x, y, z, cin0.asUInt, cin1.asUInt).map(
                _.toAFix
              )
              sumWords(i) := core.dataOut.head.asBits
                .takeLow(x.getBitsWidth)
                .asUInt
                .d(coreCount - i)
              (
                Seq(
                  core.dataOut.last.asBits.msb.d(),
                  core.dataOut.head.asBits.msb.d()
                ),
                i + 1
              )
            case TernarySubtractor2 =>
              val Seq(cin1, cin0) = carries
              val Seq(x, y, z)    = dataWords(i)
              val core            = Compressor3to1(x.getBitsWidth, 2).implH
              core.dataIn := Vec(x, y, z, cin0.asUInt, cin1.asUInt).map(
                _.toAFix
              )
              sumWords(i) := core.dataOut.head.asBits
                .takeLow(x.getBitsWidth)
                .asUInt
                .d(coreCount - i)
              (
                Seq(
                  core.dataOut.last.asBits.msb.d(),
                  core.dataOut.head.asBits.msb.d()
                ),
                i + 1
              )
          }
      }
      .last
      ._1

    val lastWord = adderType match {
      case BinaryAdder      => finalCarryOut.head.asUInt @@ sumWords.last
      case BinarySubtractor => sumWords.last
      case TernaryAdder =>
        finalCarryOut.map(_.asUInt).reduce(_ +^ _) @@ sumWords.last
      case TernarySubtractor1 =>
        ~(finalCarryOut.head ^ finalCarryOut.last).asUInt @@ sumWords.last
      case TernarySubtractor2 => sumWords.last
    }
    val outputWords = (sumWords.init :+ lastWord).map(_.asBits)

    dataOut := Vec(outputWords.reverse.reduce(_ ## _).asUInt.toAFix).d()
  }

  // TODO: is +1 necessary?
  override def latency() = coreCount + 1

  override def name = s"${className(adderType)}_$width"

  override def vivadoUtilEstimation =
    VivadoUtil(lut = width + 2, carry8 = (width + 2).divideAndCeil(8))

  override def fmaxEstimation = 600 MHz

  def sum(data: UInt*) = {
    val core = getImplH
    core.dataIn.zip(data).foreach { case (in, data) => in := data.toAFix }
    core.dataOut.head.asUInt()
  }
}
