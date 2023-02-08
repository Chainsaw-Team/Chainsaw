package Chainsaw.memory

import Chainsaw._
import Chainsaw.dsp._
import Chainsaw.arithmetic._
import Chainsaw.crypto._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus._
import spinal.lib.bus.regif._
import spinal.sim._
import spinal.core.sim._

import scala.language.postfixOps // for more simulation

// this module implement arbitrary ram by BRAM
case class RamByXilinxBram(
    w: Int,
    r: Int,
    dataWidth: Int,
    depth: Int,
    latency: Int = 2
) extends Component {

  val addrWidth = log2Up(depth)

  // interface
  val io = new Bundle {
    val readAddrs    = in(Vec.fill(r)(UInt(addrWidth bits)))
    val readDatas    = out(Vec.fill(r)(Bits(dataWidth bits)))
    val writeAddrs   = in(Vec.fill(w)(UInt(addrWidth bits)))
    val writeEnables = in(Vec.fill(w)(Bool()))
    val writeDatas   = in(Vec.fill(w)(Bits(dataWidth bits)))
  }

  // naive implementation
  val ram = Mem(HardType(Bits(dataWidth bits)), depth)
  (0 until r).foreach { i =>
    io.readDatas(i) := ram.readSync(io.readAddrs(i)).d()
  }
  (0 until w).foreach { i =>
    ram.write(io.writeAddrs(i), io.writeDatas(i), io.writeEnables(i))
  }
}

object RamByXilinxBram extends App {
  VivadoSynth(RamByXilinxBram(1, 1, 64, 16384), "bram")
  VivadoImpl(RamByXilinxBram(1, 1, 64, 16384), "bram")
}
