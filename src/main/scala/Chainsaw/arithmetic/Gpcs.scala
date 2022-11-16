package Chainsaw.arithmetic

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

import Chainsaw._
import Chainsaw.xilinx._
import Chainsaw.device._

/** list of available compressors
  *
  * @see
  *   [[Compressor4to2]]
  * @see
  *   [[Compressor3to1]]
  * @see
  *   [[Compressor3to2]]
  * @see
  *   [[Compressor6to3]]
  * @see
  *   [[Compressor606To5]]
  */
object Gpcs {
  def apply(): Seq[Compressor] = {
    val ret = Seq(Compressor1to1, Compressor4to2, Compressor3to1, Compressor6to3, Compressor3to2)
    ret
  }

}

/** general parallel counter (4; 2) for Xilinx FPGA
  *
  * @see
  *   ''Kumm, Martin & Zipf, P.. (2014). Efficient High Speed Compression Trees on Xilinx FPGAs. ''
  * @see
  *   ''Parhami, Behrooz. “Computer arithmetic - algorithms and hardware designs.” (2010).'' 8.4 PARALLEL COUNTERS AND COMPRESSORS
  */
case class Compressor4to2(width: Int) extends Component {
  val cIn                = in Bool ()
  val w, x, y, z         = in UInt (width bits)
  val sumsOut, carrysOut = out UInt (width bits)
  val cOut               = out Bool ()

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
    if (i == 0) carryChain.CI := cIn
    else carryChain.CI        := carryChains(i - 1).CO(7)
    carryChain.CI_TOP         := False
  }

  carrysOut := lutOuts.map(_._1).asBits().asUInt
  sumsOut   := carryChains.reverse.map(_.O).reduce(_ @@ _).takeLow(width).asUInt
  cOut      := carryChains.last.CO((width + 7) % 8)
}

/** @see
  *   ''Kumm, Martin & Zipf, P.. (2014). Efficient High Speed Compression Trees on Xilinx FPGAs. ''
  */
case class Compressor3to1(width: Int, sub: Int = 0) extends Component {
  val cIn0, cIn1   = in Bool ()
  val x, y, z      = in UInt (width bits)
  val sumsOut      = out UInt (width bits)
  val cOut0, cOut1 = out Bool ()

  val lutContent = sub match {
    case 0 => BigInt("69699696e8e8e8e8", 16) // x + y + z + cin0 + cin1
    case 1 => BigInt("969669698e8e8e8e", 16) // x + y - z - 1 + cin0 + cin1
    case 2 => BigInt("696996962b2b2b2b", 16) // x - y - z - 2 + cin0 + cin1
  }

  def lut(c: Bool, x: Bool, y: Bool, z: Bool) = {
    val core = LUT6_2(lutContent)
    core.I0 := x
    core.I1 := y
    core.I2 := z
    core.I3 := False
    core.I4 := c
    core.I5 := True
    (core.O5, core.O6) // O5 is carry output, O6 is XOR output
  }

  val innerCarries = Seq.fill(width + 1)(Bool())

  val lutOuts = (0 until width).map(i => lut(innerCarries(i), x(i), y(i), z(i)))
  innerCarries.head := cIn1
  innerCarries.tail.zip(lutOuts.map(_._1)).foreach { case (port, signal) =>
    port := signal
  }
  cOut1 := innerCarries.last

  val carryCount  = (width + 7) / 8
  val carryChains = Seq.fill(carryCount)(CARRY8())

  carryChains.zipWithIndex.foreach { case (carryChain, i) =>
    (0 until 8).foreach { j =>
      val index = i * 8 + j
      if (index < width) {
        carryChain.DI(j) := innerCarries(index)
        carryChain.S(j)  := lutOuts(index)._2
      } else {
        carryChain.DI(j) := False
        carryChain.S(j)  := False
      }
    }
    if (i == 0) carryChain.CI := cIn0
    else carryChain.CI        := carryChains(i - 1).CO(7)
    carryChain.CI_TOP         := False
  }

  sumsOut := carryChains.reverse.map(_.O).reduce(_ @@ _).takeLow(width).asUInt
  cOut0   := carryChains.last.CO((width + 7) % 8)
}

object Compressor4to2 extends Compressor {
  override val isFixed = false

  override val redundant: Boolean = false

  override val widthMax = 32

  override val widthMin: Int = 1

  override def inputFormat(width: Int) = 5 +: Seq.fill(width - 1)(4)

  override def outputFormat(width: Int) = 1 +: Seq.fill(width)(2)

  override def areaCost(width: Int, considerCarry8: Boolean = true, isPipeline: Boolean = true): Double =
    Seq(
      utilRequirement(width).lut.toDouble,
      if (considerCarry8) utilRequirement(width).carry8 * 8.0 else 0.0,
      if (isPipeline) utilRequirement(width).ff.toDouble / 2 else 0.0
    ).max

  override def impl(bitsIn: BitHeaps[Bool], width: Int) = {
    val paddedHeap = bitsIn.currentBitHeap.head
      .padTo(5, False) +: bitsIn.currentBitHeap.tail.map(_.padTo(4, False))
    val Seq(w, x, y, z) = (paddedHeap.head.take(4) +: paddedHeap.tail).transpose
      .map(_.asBits().asUInt)
    val cIn  = paddedHeap.head.takeRight(1).head
    val core = Compressor4to2(width)
    core.cIn := cIn
    core.w   := w
    core.x   := x
    core.y   := y
    core.z   := z
    val bitHeap = ArrayBuffer.fill(width + 1)(ArrayBuffer[Bool]())
    bitHeap.last += core.cOut
    core.sumsOut.asBools.zip(bitHeap).foreach { case (bit, column) =>
      column += bit
    }
    core.carrysOut.asBools.zip(bitHeap.tail).foreach { case (bit, column) =>
      column += bit
    }
    BitHeaps(bitHeap, bitsIn.currentWeightLow, bitsIn.currentTime)
  }

  override def utilRequirement(width: Int) =
    VivadoUtilRequirement(lut = width, carry8 = width.divideAndCeil(8), ff = outputFormat(width).sum)

  override def fMaxRequirement: HertzNumber = 600 MHz
}

/** compression by ternary adder
  */
object Compressor3to1 extends Compressor {

  override val isFixed = false

  override val redundant: Boolean = false

  override val widthMax = 16

  override val widthMin: Int = 1

  override def inputFormat(width: Int): Seq[Int] = 5 +: Seq.fill(width - 1)(3)

  override def outputFormat(width: Int): Seq[Int] = Seq.fill(width)(1) :+ 2

  override def areaCost(width: Int, considerCarry8: Boolean = true, isPipeline: Boolean = true): Double =
    Seq(
      utilRequirement(width).lut.toDouble,
      if (considerCarry8) utilRequirement(width).carry8 * 8.0 else 0.0,
      if (isPipeline) utilRequirement(width).ff.toDouble / 2 else 0.0
    ).max

  override def impl(bitsIn: BitHeaps[Bool], width: Int) = {

    def paddedHeap =
      bitsIn.currentBitHeap.head.padTo(5, False) +: bitsIn.currentBitHeap.tail
        .map(_.padTo(3, False))

    val Seq(cIn0, cIn1) = paddedHeap.head.takeRight(2)
    val Seq(x, y, z) = (paddedHeap.head.take(3) +: paddedHeap.tail).transpose
      .map(_.asBits().asUInt)
    val core = Compressor3to1(width)
    core.cIn0 := cIn0
    core.cIn1 := cIn1
    core.x    := x
    core.y    := y
    core.z    := z

    val bitHeap = ArrayBuffer.fill(width + 1)(ArrayBuffer[Bool]())
    bitHeap.last += core.cOut0
    bitHeap.last += core.cOut1
    core.sumsOut.asBools.zip(bitHeap).foreach { case (bit, column) =>
      column += bit
    }
    BitHeaps(bitHeap, bitsIn.currentWeightLow, bitsIn.currentTime)
  }

  override def utilRequirement(width: Int) =
    VivadoUtilRequirement(lut = width, carry8 = width.divideAndCeil(8), ff = outputFormat(width).sum)

  override def fMaxRequirement: HertzNumber = 600 MHz
}

object Compressor1to1 extends Compressor {

  override val isFixed = false

  override val redundant: Boolean = false

  override val widthMax = Int.MaxValue

  override val widthMin: Int = 1

  override def inputFormat(width: Int) = Seq.fill(width)(1)

  override def outputFormat(width: Int) = Seq.fill(width)(1)

  override def areaCost(width: Int, considerCarry8: Boolean = true, isPipeline: Boolean = true): Double =
    if (isPipeline) width.toDouble / 2 else 0.0

  override def impl(bitsIn: BitHeaps[Bool], width: Int): BitHeaps[Bool] =
    BitHeaps(bitsIn.newBitHeapConfigInfos.map(config => BitHeapConfigInfo(config.bitHeap, config.weightLow, config.time)): _*)

  override def utilRequirement(width: Int) = VivadoUtilRequirement(ff = width)

  override def fMaxRequirement: HertzNumber = 600 MHz
}

case class Compressor6to3() extends Component {

  val dataIn  = in UInt (6 bits)
  val dataOut = out UInt (3 bits)

  def lut(i0: Bool, i1: Bool, i2: Bool, i3: Bool, i4: Bool, i5: Bool, init: BigInt) = {
    val core = LUT6_2(init)
    core.I0 := i0
    core.I1 := i1
    core.I2 := i2
    core.I3 := i3
    core.I4 := i4
    core.I5 := i5

    core.O6 // O6 is XOR output
  }

  val lutOuts = Seq(BigInt("6996966996696996", 16), BigInt("8117177e177e7ee8", 16), BigInt("fee8e880e8808000", 16)).map(lut(dataIn(0), dataIn(1), dataIn(2), dataIn(3), dataIn(4), dataIn(5), _))
  dataOut := lutOuts.asBits().asUInt

}

object Compressor6to3 extends Compressor {

  override val isFixed = true

  override val redundant: Boolean = true

  override val widthMax = 1

  override val widthMin: Int = 1

  override def inputFormat(width: Int) = Seq(6)

  override def outputFormat(width: Int) = Seq.fill(3)(1)

  override def areaCost(width: Int, considerCarry8: Boolean = true, isPipeline: Boolean = true): Double = 3.0

  override def impl(bitsIn: BitHeaps[Bool], width: Int): BitHeaps[Bool] = {
    val dataIns = bitsIn.currentBitHeap.head.padTo(6, False).asBits().asUInt
    val core    = Compressor6to3()
    core.dataIn := dataIns
    val ret     = core.dataOut
    val bitHeap = ArrayBuffer.fill(3)(ArrayBuffer[Bool]())
    ret.asBools.zip(bitHeap).foreach { case (bit, column) => column += bit }
    BitHeaps(bitHeap, bitsIn.currentWeightLow, bitsIn.currentTime)
  }

  override def utilRequirement(width: Int) =
    VivadoUtilRequirement(lut = 3, carry8 = 0, ff = 3)

  override def fMaxRequirement: HertzNumber = 600 MHz
}

case class Compressor3to2() extends Component {
  val dataIn  = in UInt (3 bits)
  val dataOut = out UInt (2 bits)

  def lut(x: Bool, y: Bool, z: Bool) = {
    val core = LUT6_2(BigInt("96969696e8e8e8e8", 16))
    core.I0 := x
    core.I1 := y
    core.I2 := z
    core.I3 := False
    core.I4 := False
    core.I5 := True
    (core.O5, core.O6) // O5 is carry output, O6 is XOR output
  }

  val lutOuts = lut(dataIn(0), dataIn(1), dataIn(2))
  dataOut := (lutOuts._1 ## lutOuts._2).asUInt
}

/** full adder with carry
  */
object Compressor3to2 extends Compressor {

  override val isFixed = true

  override val redundant: Boolean = false

  override val widthMax = 1

  override val widthMin = 1

  override def inputFormat(width: Int) = Seq(3)

  override def outputFormat(width: Int): Seq[Int] = Seq.fill(2)(1)

  override def areaCost(width: Int, considerCarry8: Boolean = true, isPipeline: Boolean = true): Double = 1.0

  override def impl(bitsIn: BitHeaps[Bool], width: Int): BitHeaps[Bool] = {
    val dataIns = bitsIn.currentBitHeap.head.padTo(3, False)
    val ret     = Compressor3to2()
    ret.dataIn := dataIns.asBits().asUInt
    val bitHeap = ArrayBuffer.fill(2)(ArrayBuffer[Bool]())
    ret.dataOut.asBools.zip(bitHeap).foreach { case (bit, column) =>
      column += bit
    }
    BitHeaps(bitHeap, bitsIn.currentWeightLow, bitsIn.currentTime)
  }

  override def utilRequirement(width: Int): VivadoUtil =
    VivadoUtilRequirement(lut = 1, carry8 = 0, ff = 2)

  override def fMaxRequirement: HertzNumber = 600 MHz
}

case class Compressor606To5() extends Component {
  val dataIn  = in Vec (UInt(6 bits), 2)
  val dataOut = out UInt (5 bits)

  def lut(init: BigInt, i0: Bool, i1: Bool, i2: Bool, i3: Bool, i4: Bool, i5: Bool) = {
    val core = LUT6_2(init)
    core.I0 := i0
    core.I1 := i1
    core.I2 := i2
    core.I3 := i3
    core.I4 := i4
    core.I5 := i5
    (core.O5, core.O6) // O5 is carry output, O6 is XOR output
  }

  val lutIns = Seq(dataIn(0).asBools.init :+ True, dataIn(0).asBools.take(4) ++ Seq(False, True), dataIn(1).asBools, dataIn(1).asBools.take(4) ++ Seq(dataIn(1)(5), True))
  val lutOuts = Seq(BigInt("9669699696696996", 16), BigInt("7ee87ee8e8e8e8e8", 16), BigInt("6996966996696996", 16), BigInt("177e7ee8e8e8e8e8", 16))
    .zip(lutIns)
    .map { case (init, ins) =>
      lut(init, ins(0), ins(1), ins(2), ins(3), ins(4), ins(5))
    }

  val dataIns =
    (Seq(dataIn(0)(4), lutOuts(1)._1, dataIn(1)(4), lutOuts(3)._1) ++ Seq.fill(4)(False)).asBits.asUInt
  val selects = (Seq(lutOuts.head._2, lutOuts(1)._2, lutOuts(2)._2, lutOuts(3)._2) ++ Seq.fill(4)(False)).asBits.asUInt
  val carry8  = CARRY8()
  carry8.CI     := dataIn(0).asBools.last
  carry8.CI_TOP := False
  carry8.DI     := dataIns
  carry8.S      := selects
  dataOut       := (Seq.tabulate(4)(carry8.O(_)) :+ carry8.CO(3)).asBits.asUInt
}

object Compressor606To5 extends Compressor {

  override val isFixed: Boolean = true

  override val redundant: Boolean = true

  override val widthMax: Int = 1

  override val widthMin: Int = 1

  /** number of bits in input columns, low to high
    */
  override def inputFormat(width: Int): Seq[Int] = Seq(6, 0, 6)

  /** number of bits in output columns, low to high
    */
  override def outputFormat(width: Int): Seq[Int] = Seq.fill(5)(1)

  /** number of LUTs
    */
  override def areaCost(width: Int, considerCarry8: Boolean = true, isPipeline: Boolean = true): Double =
    if (considerCarry8) 8.0 else 4.0

  /** hardware implementation, the compressor is responsible for padding zeros
    */
  override def impl(bitsIn: BitHeaps[Bool], width: Int): BitHeaps[Bool] = {
    val dataIns = ArrayBuffer(bitsIn.currentBitHeap.head.padTo(6, False), bitsIn.currentBitHeap.last.padTo(6, False))
    val ret     = Compressor606To5()
    ret.dataIn := Vec(dataIns.head.asBits.asUInt, dataIns.last.asBits.asUInt)
    val bitHeap = ArrayBuffer.fill(5)(ArrayBuffer[Bool]())
    bitHeap.zip(ret.dataOut.asBools).foreach { case (bits, bout) =>
      bits += bout
    }
    BitHeaps(bitHeap, bitsIn.currentWeightLow, bitsIn.currentTime)
  }

  override def utilRequirement(width: Int): VivadoUtil = VivadoUtilRequirement(lut = 4, carry8 = 1, ff = 5)

  override def fMaxRequirement: HertzNumber = 600 MHz
}
