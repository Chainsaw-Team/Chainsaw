package Chainsaw.memory

import Chainsaw._            // for basic templates
import Chainsaw.dsp._        // for dsp operators
import Chainsaw.arithmetic._ // for arithmetic operators
import Chainsaw.crypto._     // for crypto operators
import Chainsaw.xilinx._     // for Xilinx FPGA Flow

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._       // for finite state machine dialect
import spinal.lib.bus._       // for all kinds of bus and regIf
import spinal.lib.bus.regif._ // for regIf
import spinal.sim._           // for simulation
import spinal.core.sim._      // for more simulation
import Chainsaw.xilinx._

import Chainsaw.memory.RamPortType._

/** @see
  *   ''Efficient Multi-Ported Memories for FPGAs'' [[https://dl.acm.org/doi/10.1145/1723112.1723122]]
  * @param m
  *   number of write ports
  * @param n
  *   number of read ports
  * @param mode
  *   implementation mode
  */
case class MwNrRam(m: Int, n: Int, mode: MwNrMode) extends Component {

  val stageNum = m + n
  val io = new Bundle {
    val reads  = Seq.fill(n)(slave(XILINX_BRAM_PORT(18, 10, READ)))
    val writes = Seq.fill(m)(slave(XILINX_BRAM_PORT(18, 10, WRITE)))
  }

  mode match {
    case PURELOGIC => {
      val ram = Vec(Reg(Bits(18 bits)), 1024)
      io.writes.foreach { port =>
        when(port.en && port.we)(ram(port.addr) := port.dataIn)
      }
      io.reads.foreach(_.dataOut.assignDontCare())
      io.reads.foreach { port =>
        when(port.en)(port.dataOut := RegNext(ram(port.addr)))
      }
    }
    case REPLICATION => {
      require(m == 1, "replication cannot implement multiple write ports")
      val rams = Seq.fill(n)(XILINX_BRAM18E2_PREASSIGNED())
      // SDP mode
      rams.foreach { ram => ram.ioB << io.writes.head } // write
      rams.zip(io.reads).foreach { case (ram, port) =>
        val ramPort = ram.ioA
        ramPort.addr := port.addr
        ramPort.en   := True
        ramPort.we   := False
        port.dataOut := ramPort.dataOut
      } // read
    }
    case MULTIPUMPING => {
      io.reads.foreach(_.dataOut.clearAll())
      io.reads.foreach(_.dataOut.allowOverride)
      val multiplexCounter = CounterFreeRun(stageNum)
      val ram              = XILINX_BRAM18E2()

      ram.ioA.preAssign()
      ram.ioB.preAssign()

      val writeAddr = io.writes.map(port => RegNext(port.addr))
      val writeData = io.writes.head.dataIn +: io.writes.tail.map(port => RegNext(port.dataIn))
      val readAddr  = io.reads.head.addr +: io.reads.tail.map(port => RegNext(port.addr))
      val readData  = io.reads.init.map(port => Reg(port.dataOut)) :+ io.reads.last.dataOut

      Seq(writeAddr, writeData, readAddr, readData).foreach(_.foreach(_.addTag(crossClockDomain)))

      io.reads.init.zip(readData.init).foreach { case (port, reg) => port.dataOut := reg }

      switch(multiplexCounter.value) {

        (0 until n).foreach { readIndex =>
          is(U((readIndex + 1) % stageNum)) {
            ram.ioA.addr                                := readAddr(readIndex)
            ram.ioA.en                                  := True
            ram.ioA.we                                  := False
            if (readIndex != 0) readData(readIndex - 1) := ram.ioA.dataOut
          }
        }
        (0 until m).foreach { writeIndex =>
          is(U((writeIndex + n + 1) % stageNum)) {
            ram.ioA.doWrite(writeAddr(writeIndex), writeData(writeIndex))
            if (writeIndex == 0) readData(n - 1) := ram.ioA.dataOut
          }
        }
      }
    }
    //    case LVT => {
    //      val rams = Seq.tabulate(m, n)((_, _) => XILINX_BRAM18E2_PREASSIGNED()) // m bank, each has n rams
    //      val lvt = mWnRRAM(m, n, PURELOGIC)
    //    }
  }
}

case class mWnRDUT(m: Int, n: Int, mode: MwNrMode) extends Component {
  val ramClockDomain    = ClockDomain.external("ram", config = ClockDomainConfig(resetKind = BOOT))
  val globalClockDomain = ClockDomain.external("global", config = xilinxCDConfig)

  val globalClokingArea = new ClockingArea(globalClockDomain) {
    val io = new Bundle {
      val writes   = Seq.fill(m)(slave(XILINX_BRAM_PORT(18, 10, WRITE)))
      val reads    = Seq.fill(n)(slave(XILINX_BRAM_PORT(18, 10, READ)))
      val forClock = out Bool ()
    }

    mode match {
      case MULTIPUMPING => {
        val ramClockingArea = new ClockingArea(ramClockDomain) {
          val mem = MwNrRam(m, n, mode) // declared with ramClockDomain
        }
        io.writes.zip(ramClockingArea.mem.io.writes).foreach { case (outer, inner) => outer <> inner }
        io.reads.zip(ramClockingArea.mem.io.reads).foreach { case (outer, inner) => outer <> inner }
      }
      case _ => {
        val mem = MwNrRam(m, n, mode) // declared with globalClockDomain
        io.writes.zip(mem.io.writes).foreach { case (outer, inner) => outer <> inner }
        io.reads.zip(mem.io.reads).foreach { case (outer, inner) => outer <> inner }
      }
    }

    io.forClock := RegNext(io.reads(0).dataOut.lsb)
  }
}

object MwNrRam {
  def main(args: Array[String]): Unit = {
    VivadoImpl(MwNrRam(2, 2, PURELOGIC), "ram2w2r")
  }
}
