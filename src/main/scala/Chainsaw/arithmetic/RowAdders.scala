package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.device.{CARRY8, LUT5to2, LUT6_2}
import Chainsaw.xilinx.VivadoUtil
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.math.{BigInt, log}

abstract class RowAdder extends CompressorGenerator  {

  def width: Int

  def widthMax: Int

  def widthMin: Int

  require(
    width >= widthMin && width <= widthMax,
    s"compressor width out of range, require: [$widthMin, $widthMax], actual: $width"
  )

  def name =
    s"${className(this)}_${width}_${if (shouldDoComplement) hashName(getComplementHeap)
    else "noComplement"}"

  def inputInfos = columns2Infos(inputFormat)

  def outputInfos = columns2Infos(outputFormat)

  override def inputTypes: Seq[NumericType] =
    inputInfos.map(_.width).map(NumericType.U)

  override def outputTypes: Seq[NumericType] =
    outputInfos.map(_.width).map(NumericType.U)

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = {
    val ret = yours
      .zip(outputInfos)
      .map { case (decimal, info) => info.eval(decimal.toBigInt()) }
      .sum
    if (ret != golden.head.toBigInt())
      println(s"diff = ${golden.head.toBigInt() - ret}")
    ret == golden.head.toBigInt()
  }

}

case class Compressor4to2(
    width: Int,
    override val complementHeap: Seq[Seq[Boolean]] = null
) extends RowAdder {

  override def widthMax = cpaWidthMax

  override def widthMin = 8

  override def inputFormat = 5 +: Seq.fill(width - 1)(4)

  override def outputFormat = 1 +: Seq.fill(width)(2)

  override def implH = new ChainsawOperatorModule(this) {
    val Seq(w, x, y, z, cIn) = dataIn.map(_.asUInt())

    val lutGen = LUT5to2(
      (
          i0,
          i1,
          i2,
          i3,
          i4
      ) => (i0.toInt + i1.toInt + i2.toInt) >= 2, // carryout bit of FA
      (i0, i1, i2, i3, i4) => i0 ^ i1 ^ i2 ^ i3 ^ i4 // sum bit of FA
    )

    val lutOuts = (0 until width)
      .map(i => lutGen.process(x(i), y(i), z(i), w(i), False))
      .map(seq => (seq(0), seq(1)))

    val carryCount  = (width + 7) / 8
    val carryChains = Seq.fill(carryCount)(CARRY8())
    val selects     = lutOuts.map(_._2)
    val data        = w.asBits

    carryChains.zipWithIndex.foreach { case (carryChain, i) =>
      (0 until 8).foreach { j =>
        val index = i * 8 + j
        if (index < width) {
          carryChain.DI(j) := data(index)
          carryChain.S(j)  := selects(index)
        } else {
          carryChain.DI(j) := False
          carryChain.S(j)  := False
        }
      }
      if (i == 0) carryChain.CI := cIn.asBool
      else carryChain.CI        := carryChains(i - 1).CO(7)
      carryChain.CI_TOP         := False
    }

    dataOut := Vec(
      (carryChains.last.CO((width + 7) % 8) ## carryChains.reverse
        .map(_.O)
        .reduce(_ @@ _)
        .takeLow(width)).asUInt.toAFix,        // weight = 0
      lutOuts.map(_._1).asBits().asUInt.toAFix // weight = 1
    )
  }

  // TODO: implement this for faster simulation
  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    val sum = dataIn.reduce(_ + _).asUInt().resize(width + 2)
    // this trick assure that both data won't overflow
    dataOut(0) := (((sum.takeHigh(width + 1).asUInt - sum
      .takeHigh(width)
      .asUInt) << 1).resize(width + 1) + sum.lsb.asUInt).toAFix
    dataOut(1) := sum.takeHigh(width).asUInt.toAFix
  })

  override def vivadoUtilEstimation =
    VivadoUtil(
      lut    = width,
      carry8 = width.divideAndCeil(8),
      ff     = outputFormat.sum
    )
}

// TODO: 3to1 and 2to1
case class Compressor3to1(
    width: Int,
    mode: Int,
    override val complementHeap: Seq[Seq[Boolean]] = null
) extends RowAdder {
  override def widthMax = cpaWidthMax

  override def widthMin = 8

  override def inputFormat = 5 +: Seq.fill(width - 1)(3)

  override def outputFormat = Seq.fill(width)(1) :+ 2

  override def vivadoUtilEstimation =
    VivadoUtil(
      lut    = width,
      carry8 = width.divideAndCeil(8),
      ff     = outputFormat.sum
    )

  override def implH = new ChainsawOperatorModule(this) {
    val Seq(x, y, z, cIn1, cIn0) = dataIn.map(_.asUInt())

    val lutContent = mode match {
      case 0 => BigInt("69699696e8e8e8e8", 16) // x + y + z + cin0 + cin1
      case 1 => BigInt("969669698e8e8e8e", 16) // x + y - z - 1 + cin0 + cin1
      case 2 => BigInt("696996962b2b2b2b", 16) // x - y - z - 2 + cin0 + cin1
    }

    val innerCarries    = Seq.fill(width + 1)(Bool())
    val cinAfterInverse = Bool()

    // TODO: simplify this part
    // TODO: reduce the number of NOT gate by sorting
    val lutOuts = (0 until width).map { i =>
      if (i == 0) {
        val candidates  = Seq(x(i), y(i), z(i), cIn1.asBool, cIn0.asBool)
        val inverseList = getComplementHeap.head.padTo(5, true).map(!_)
        val extra = inverseList
          .drop(3)
          .count(_ == true) // every LUT can take care of 3 complement bit
        if (extra > 0) logger.info(s"$extra extra not gate for complement bits")
        require(
          inverseList.length == candidates.length,
          s"3to1 compressor requires ${candidates.length} inverse bits, but only ${inverseList.length} given"
        )
        // for carry
        innerCarries.head := (if (inverseList(3)) !candidates(3)
                              else candidates(3))
        cinAfterInverse := (if (inverseList(4)) !candidates(4)
                            else candidates(4))
        // for LUT
        val bits = candidates.take(3) :+ False :+ candidates(3) :+ True
        val inverse: Seq[Boolean] =
          inverseList.take(3) :+ false :+ inverseList(3) :+ false
        LUT6_2.process(bits, LUT6_2.getValueWithInverse(lutContent, inverse))
      } else {
        val candidates = Seq(x(i), y(i), z(i), False, innerCarries(i), True)
        val inverseList =
          getComplementHeap(i).padTo(3, true).map(!_) ++ Seq.fill(3)(false)
        LUT6_2.process(
          candidates,
          LUT6_2.getValueWithInverse(lutContent, inverseList)
        )
      }
    }
    innerCarries.tail.zip(lutOuts.map(_.head)).foreach { case (port, signal) =>
      port := signal
    }
    val carryCount  = (width + 7) / 8
    val carryChains = Seq.fill(carryCount)(CARRY8())

    carryChains.zipWithIndex.foreach { case (carryChain, i) =>
      (0 until 8).foreach { j =>
        val index = i * 8 + j
        if (index < width) {
          carryChain.DI(j) := innerCarries(index)
          carryChain.S(j)  := lutOuts(index).last
        } else {
          carryChain.DI(j) := False
          carryChain.S(j)  := False
        }
      }
      if (i == 0) carryChain.CI := cinAfterInverse
      else carryChain.CI        := carryChains(i - 1).CO(7)
      carryChain.CI_TOP         := False
    }

    dataOut := Vec(
      (innerCarries.last ## carryChains.reverse
        .map(_.O)
        .reduce(_ @@ _)
        .takeLow(width)).asUInt.toAFix, // weight = 0
      carryChains.last.CO((width + 7) % 8).asUInt.toAFix
    )
  }

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    val sum = dataIn.reduce(_ + _).asUInt().resize(width + 2)
    dataOut := Vec(
      (((sum.takeHigh(2).asUInt - sum.takeHigh(1).asUInt) << width)
        .resize(width + 1) + sum.takeLow(width).asUInt).toAFix,
      sum.takeHigh(1).asUInt.toAFix
    )
  })
}

object Compressor3to1 {
  def apply(width: Int, inverseHeap: Seq[Seq[Boolean]]): Compressor3to1 =
    Compressor3to1(width, 0, inverseHeap)

  def apply(width: Int): Compressor3to1 = Compressor3to1(width, 0)
}

case class Compressor1to1(
    width: Int,
    override val complementHeap: Seq[Seq[Boolean]] = null
) extends RowAdder {
  override def widthMax = cpaWidthMax

  override def widthMin = 1

  override def inputFormat = Seq.fill(width)(1)

  override def outputFormat = Seq.fill(width)(1)

  override def vivadoUtilEstimation = VivadoUtil(lut = 0, ff = outputFormat.sum)

  override def implH = new ChainsawOperatorModule(this) {
    dataOut := dataIn
  }

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    dataOut := dataIn
  })
}

object RowAdders {
  //  def apply(): Seq[RowAdder] = Seq(Compressor1to1(1), Compressor3to1(cpaWidthMax), Compressor4to2(cpaWidthMax))
  def apply(): Seq[RowAdder] =
    Seq(Compressor1to1(1), Compressor3to1(cpaWidthMax))
}
