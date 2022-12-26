package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.device.{CARRY8, LUT6_2}
import Chainsaw.xilinx.VivadoUtilEstimation
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

abstract class RowAdder extends CompressorGenerator {

  def width: Int

  def widthMax: Int

  def widthMin: Int

  require(width >= widthMin && width <= widthMax, s"compressor width out of range, require: [$widthMin, $widthMax], actual: $width")

  def name = s"${className(this)}_$width"

  def inputInfos = columns2Infos(inputFormat)

  def outputInfos = columns2Infos(outputFormat)

  override def inputTypes: Seq[NumericType] = inputInfos.map(_.width).map(NumericType.U)

  override def outputTypes: Seq[NumericType] = outputInfos.map(_.width).map(NumericType.U)

  override def impl(testCase: TestCase) = Seq(testCase.data.sum).padTo(outputTypes.length, BigDecimal(0))

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    val ret = yours.zip(outputInfos).map { case (decimal, info) => info.eval(decimal.toBigInt()) }.sum
    ret == golden.head.toBigInt()
  }

  override def testCases = Seq.fill(100)(randomTestCase)

  // TODO: heap -> UInt and UInt -> heap should be implemented by the bit heap
  override def compress(bitsIn: BitHeapHard): BitHeapHard = {
    val paddedBitsIn = bitsIn.zip(inputFormat).map { case (bits, h) => bits.padTo(h, False) }
    val operands = columns2Operands(paddedBitsIn)
    val core = getImplH
    core.dataIn := operands
    operands2Columns(core.dataOut, outputFormat).asInstanceOf[BitHeapHard]
  }
}

case class Compressor4to2(width: Int) extends RowAdder {

  override def widthMax = cpaWidthMax

  override def widthMin = 8

  override def inputFormat = 5 +: Seq.fill(width - 1)(4)

  override def outputFormat = 1 +: Seq.fill(width)(2)

  override def implH = new ChainsawOperatorModule(this) {
    dataOut := Compressor4to2Primitive.primitiveCompress(width)(dataIn)
  }

  // TODO: implement this for faster simulation
  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    val sum = dataIn.reduce(_ + _).asUInt().resize(width + 2)
    // this trick assure that both data won't overflow
    dataOut(0) := (((sum.takeHigh(width + 1).asUInt - sum.takeHigh(width).asUInt) << 1).resize(width + 1) + sum.lsb.asUInt).toAFix
    dataOut(1) := sum.takeHigh(width).asUInt.toAFix
  })

  override def vivadoUtilEstimation =
    VivadoUtilEstimation(lut = width, carry8 = width.divideAndCeil(8), ff = 0)
}

// TODO: 3to1 and 2to1
case class Compressor3to1(width: Int, mode: Int) extends RowAdder {
  override def widthMax = cpaWidthMax

  override def widthMin = 8

  override def inputFormat = 5 +: Seq.fill(width - 1)(3)

  override def outputFormat = Seq.fill(width)(1) :+ 2

  override def vivadoUtilEstimation =
    VivadoUtilEstimation(lut = width, carry8 = width.divideAndCeil(8), ff = outputFormat.sum)

  override def implH = new ChainsawOperatorModule(this) {
    dataOut := Compressor3to1Primitive.primitiveCompress(width, mode)(dataIn)
  }

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    val sum = dataIn.reduce(_ + _).asUInt().resize(width + 2)
    dataOut := Vec((((sum.takeHigh(2).asUInt - sum.takeHigh(1).asUInt) << width).resize(width + 1) + sum.takeLow(width).asUInt).toAFix, sum.takeHigh(1).asUInt.toAFix)
  })
}

object Compressor3to1 {
  def apply(width: Int): Compressor3to1 = Compressor3to1(width, 0)
}

case class Compressor1to1(width: Int) extends RowAdder {
  override def widthMax = cpaWidthMax

  override def widthMin = 1

  override def inputFormat = Seq.fill(width)(1)

  override def outputFormat = Seq.fill(width)(1)

  override def vivadoUtilEstimation = VivadoUtilEstimation(lut = 0, ff = width)

  override def implH = new ChainsawOperatorModule(this) {
    dataOut := dataIn
  }

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    dataOut := dataIn
  })
}

object RowAdders {
  def apply(): Seq[RowAdder] = Seq(Compressor1to1(1), Compressor3to1(cpaWidthMax), Compressor4to2(cpaWidthMax))
}
