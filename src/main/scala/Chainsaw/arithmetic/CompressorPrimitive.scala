package Chainsaw.arithmetic

import spinal.core._
import spinal.lib._
import Chainsaw.device._
import scala.math._

trait CompressorPrimitive {
  def lut(i0: Bool, i1: Bool, i2: Bool, i3: Bool, i4: Bool, i5: Bool, init: BigInt) = {
    val core = LUT6_2(init)
    core.I0 := i0
    core.I1 := i1
    core.I2 := i2
    core.I3 := i3
    core.I4 := i4
    core.I5 := i5
    (core.O5, core.O6) // O5 is carry output, O6 is XOR output
  }
}

trait GpcPrimitive extends CompressorPrimitive {
  def primitiveCompress: Vec[AFix] => Vec[AFix]
}

trait RowAdderPrimitive extends CompressorPrimitive {
  def primitiveCompress(width: Int, mode: Int = 0): Vec[AFix] => Vec[AFix]
}

object Compressor6to3Primitive extends GpcPrimitive {
  override def primitiveCompress: Vec[AFix] => Vec[AFix] = (dataIn: Vec[AFix]) => {
    val dataBitsIn = dataIn.map(_.asBits).head
    val lutOuts = Seq(BigInt("6996966996696996", 16), BigInt("8117177e177e7ee8", 16), BigInt("fee8e880e8808000", 16))
      .map(lut(dataBitsIn(0), dataBitsIn(1), dataBitsIn(2), dataBitsIn(3), dataBitsIn(4), dataBitsIn(5), _)._2)
    Vec(lutOuts.asBits().asUInt.toAFix)
  }
}

object Compressor3to2Primitive extends GpcPrimitive {
  override def primitiveCompress: Vec[AFix] => Vec[AFix] = (dataIn: Vec[AFix]) => {
    val dataBitsIn = dataIn.map(_.asBits).head
    val lutOuts = lut(dataBitsIn(0), dataBitsIn(1), dataBitsIn(2), False, False, True, BigInt("96969696e8e8e8e8", 16))
    Vec((lutOuts._1 ## lutOuts._2).asUInt.toAFix)
  }
}

object Compressor606to5Primitive extends GpcPrimitive {
  override def primitiveCompress: Vec[AFix] => Vec[AFix] = (dataIn: Vec[AFix]) => {
    val dataBitsIn = dataIn.map(_.asBits)
    val lutIns = Seq(
      dataBitsIn(0).asBools.init :+ True,
      dataBitsIn(0).asBools.take(4) ++ Seq(False, True),
      dataBitsIn(1).asBools,
      dataBitsIn(1).asBools.take(4) ++ Seq(dataBitsIn(1)(5), True)
    )
    val lutOuts = Seq(BigInt("9669699696696996", 16), BigInt("7ee87ee8e8e8e8e8", 16), BigInt("6996966996696996", 16), BigInt("177e7ee8e8e8e8e8", 16))
      .zip(lutIns)
      .map { case (init, ins) =>
        lut(ins(0), ins(1), ins(2), ins(3), ins(4), ins(5), init)
      }

    val dataIns =
      (Seq(dataBitsIn(0)(4), lutOuts(1)._1, dataBitsIn(1)(4), lutOuts(3)._1) ++ Seq.fill(4)(False)).asBits.asUInt
    val selects = (Seq(lutOuts.head._2, lutOuts(1)._2, lutOuts(2)._2, lutOuts(3)._2) ++ Seq.fill(4)(False)).asBits.asUInt
    val carry8 = CARRY8()
    carry8.CI := dataBitsIn(0).asBools.last
    carry8.CI_TOP := False
    carry8.DI := dataIns
    carry8.S := selects
    Vec((Seq.tabulate(4)(carry8.O(_)) :+ carry8.CO(3)).asBits.asUInt.toAFix)
  }
}

object Compressor4to2Primitive extends RowAdderPrimitive {
  override def primitiveCompress(width: Int, mode: Int = 0): Vec[AFix] => Vec[AFix] = (dataIn: Vec[AFix]) => {
    val Seq(w, x, y, z, cIn) = dataIn.map(_.asUInt())

    val lutGen = LUT5to2(
      (i0, i1, i2, i3, i4) => (i0.toInt + i1.toInt + i2.toInt) >= 2, // carryout bit of FA
      (i0, i1, i2, i3, i4) => i0 ^ i1 ^ i2 ^ i3 ^ i4 // sum bit of FA
    )

    val lutOuts = (0 until width).map(i => lutGen.process(x(i), y(i), z(i), w(i), False))
      .map(seq => (seq(0), seq(1)))

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

    Vec(
      (carryChains.last.CO((width + 7) % 8) ## carryChains.reverse
        .map(_.O)
        .reduce(_ @@ _)
        .takeLow(width)).asUInt.toAFix, // weight = 0
      lutOuts.map(_._1).asBits().asUInt.toAFix // weight = 1
    )
  }
}

object Compressor3to1Primitive extends RowAdderPrimitive {
  override def primitiveCompress(width: Int, mode: Int = 0): Vec[AFix] => Vec[AFix] = (dataIn: Vec[AFix]) => {
    val Seq(x, y, z, cIn1, cIn0) = dataIn.map(_.asUInt())

    val lutContent = mode match {
      case 0 => BigInt("69699696e8e8e8e8", 16) // x + y + z + cin0 + cin1
      case 1 => BigInt("969669698e8e8e8e", 16) // x + y - z - 1 + cin0 + cin1
      case 2 => BigInt("696996962b2b2b2b", 16) // x - y - z - 2 + cin0 + cin1
    }

    val innerCarries = Seq.fill(width + 1)(Bool())
    val lutOuts = (0 until width).map(i => lut(x(i), y(i), z(i), False, innerCarries(i), True, lutContent))
    innerCarries.head := cIn0.asBools.head
    innerCarries.tail.zip(lutOuts.map(_._1)).foreach { case (port, signal) =>
      port := signal
    }

    val carryCount = (width + 7) / 8
    val carryChains = Seq.fill(carryCount)(CARRY8())

    carryChains.zipWithIndex.foreach { case (carryChain, i) =>
      (0 until 8).foreach { j =>
        val index = i * 8 + j
        if (index < width) {
          carryChain.DI(j) := innerCarries(index)
          carryChain.S(j) := lutOuts(index)._2
        } else {
          carryChain.DI(j) := False
          carryChain.S(j) := False
        }
      }
      if (i == 0) carryChain.CI := cIn1.asBools.head
      else carryChain.CI := carryChains(i - 1).CO(7)
      carryChain.CI_TOP := False
    }

    Vec(
      (innerCarries.last ## carryChains.reverse.map(_.O).reduce(_ @@ _).takeLow(width)).asUInt.toAFix, // weight = 0
      carryChains.last.CO((width + 7) % 8).asUInt.toAFix
    )
  }
}
