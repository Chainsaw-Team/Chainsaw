package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.device.{CARRY8, LUT6_2}
import Chainsaw.xilinx.VivadoUtilEstimation
import spinal.core._
import spinal.lib._

abstract class RowAdder extends ChainsawOperatorGenerator with Compressor {

  val width: Int
  val widthMax: Int
  val widthMin: Int

  def name = s"${className(this)}_$width"

  private def columns2Infos(columns: Seq[Int]) = {
    (0 until columns.max).map(i => ArithInfo(
      width = columns.count(_ > i),
      weight = columns.indexWhere(_ > i)))
  }

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
  override def compress(bitsIn: BitHeapHard) = ???
}

case class Compressor4to2(width: Int) extends RowAdder {

  override val widthMax = cpaWidthMax
  override val widthMin = 8

  override def inputFormat = 5 +: Seq.fill(width - 1)(4)

  override def outputFormat = 1 +: Seq.fill(width)(2)

  override def implH = new ChainsawOperatorModule(this) {
    val Seq(w, x, y, z, cIn) = dataIn.map(_.asUInt())

    def lut(w: Bool, x: Bool, y: Bool, z: Bool) = {
      val core = LUT6_2(BigInt("69966996e8e8e8e8", 16))
      core.I0 := x
      core.I1 := y
      core.I2 := z
      core.I3 := w
      core.I4 := False
      core.I5 := True
      (core.O5, core.O6) // O5 is carry output, O6 is XOR output
    }

    val lutOuts = (0 until width).map(i => lut(w(i), x(i), y(i), z(i)))

    val carryCount = (width + 7) / 8
    val carryChains = Seq.fill(carryCount)(CARRY8())
    val selects = lutOuts.map(_._2)
    val data = w.asBits

    carryChains.zipWithIndex.foreach { case (carryChain, i) =>
      (0 until 8).foreach { j =>
        val index = i * 8 + j
        if (index < width) {
          carryChain.DI(j) := data(index)
          carryChain.S(j) := selects(index)
        } else {
          carryChain.DI(j) := False
          carryChain.S(j) := False
        }
      }
      if (i == 0) carryChain.CI := cIn.asBool
      else carryChain.CI := carryChains(i - 1).CO(7)
      carryChain.CI_TOP := False
    }

    dataOut(0) := (carryChains.last.CO((width + 7) % 8) ## carryChains.reverse.map(_.O).reduce(_ @@ _)
      .takeLow(width)).asUInt.toAFix // weight = 0
    dataOut(1) := lutOuts.map(_._1).asBits().asUInt.toAFix // weight = 1
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
case class Compressor3to1(width: Int) extends RowAdder {
  override val widthMax = cpaWidthMax
  override val widthMin = 8

  override def inputFormat = ???

  override def outputFormat = ???

  override def vivadoUtilEstimation = ???

  override def implH = ???

  override def implNaiveH = ???
}