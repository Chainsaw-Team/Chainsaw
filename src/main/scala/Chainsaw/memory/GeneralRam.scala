package Chainsaw.memory

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

// TODO: a unified test for RAMs


/** multi-ported RAM based on SingleRAM/DoubleRAM
 */
case class GeneralRam(
                       width: Int,
                       depth: Int,
                       readPort: Int,
                       writePort: Int,
                       readLatency: Int,
                       w2rLatency: Int,
                       ramType: RamType
                     ) extends Component {

}

/** 1W1R RAM with arbitrary width and depth
  */
case class SingleRam(width: Int, depth: Int, readLatency: Int, w2rLatency: Int) extends Component {

  val writeLatency = w2rLatency - readLatency

  def estimation = {
    VivadoUtil(width.divideAndCeil(72) * depth.divideAndCeil(1 << 12))
  }

  val readAddr = in UInt (log2Up(depth) bits)
  val readData = out Bits (width bits)

  val writeAddr = in UInt (log2Up(depth) bits)
  val writeData = in Bits (width bits)
  val writeEn   = in Bool ()

  val mem = Mem(Bits(width bits), depth)
  mem.setAsUltraRam()
  readData := mem.readSync(readAddr).d(readLatency - 1)
  mem.write(
    writeAddr.d(writeLatency),
    writeData.d(writeLatency),
    writeEn.d(writeLatency)
  )
}

/** dual-port RAM with arbitrary width and depth
  */
case class DoubleRam(width: Int, depth: Int, readLatency: Int, w2rLatency: Int) extends Component {

  val writeLatency = w2rLatency - readLatency

  def estimation = {
    VivadoUtil(width.divideAndCeil(72) * depth.divideAndCeil(1 << 12))
  }

  val readAddr = in Vec (UInt(log2Up(depth) bits), 2)
  val readData = out Vec (Bits(width bits), 2)

  val writeAddr = in Vec (UInt(log2Up(depth) bits), 2)
  val writeData = in Vec (Bits(width bits), 2)
  val writeEn   = in Vec (Bool(), 2)

  val mem = Mem(Bits(width bits), depth)
  mem.setAsBlockRam()
  readData.zip(readAddr).foreach { case (data, addr) =>
    data := mem.readSync(addr).d(readLatency - 1)
  }
  writeAddr.indices.foreach(i =>
    mem.write(
      writeAddr(i).d(writeLatency),
      writeData(i).d(writeLatency),
      writeEn(i).d(writeLatency)
    )
  )
}

