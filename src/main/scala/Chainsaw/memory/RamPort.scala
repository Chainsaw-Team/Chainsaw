package Chainsaw.memory
object RamPortType extends Enumeration {
  type RamPortType = Value
  val READ, WRITE, READWRITE = Value
}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._ // for finite state machine dialect
import spinal.lib.bus._ // for all kinds of bus and regIf
import spinal.lib.bus.regif._ // for regIf
import spinal.sim._ // for simulation
import spinal.core.sim._ // for more simulation

import RamPortType._

case class XILINX_BRAM_PORT(dataWidth: Int, addressWidth: Int, portType: RamPortType = READWRITE)
    extends Bundle
    with IMasterSlave { // FIXME: may not be needed
  val hasRead  = portType != WRITE
  val hasWrite = portType != READ

  val addr    = UInt(addressWidth bits)
  val dataIn  = if (hasWrite) Bits(dataWidth bits) else null
  val dataOut = if (hasRead) Bits(dataWidth bits) else null
  val en      = Bool
  val we      = if (portType != READ) Bool else null

  override def asMaster(): Unit = { // implemented as slave on a RAM, as master on a ram reader/writer
    out(addr, en)
    if (portType != WRITE) in(dataOut)
    if (portType != READ) out(dataIn, we)
  }

  def >>(that: XILINX_BRAM_PORT): Unit = { // outer >> inner, the direction is the direction of control flow, not data flow
    require(
      !((this.portType == READ && that.portType == WRITE) || (this.portType == WRITE && that.portType == READ)),
      "read/write port cannot be driven by write/read port"
    )

    that.addr := addr
    that.en   := en

    if (this.portType != READ && that.portType != READ) that.dataIn := dataIn
    if (this.portType != WRITE && that.portType != WRITE) dataOut   := that.dataOut
    if (this.portType != READ && that.portType != READ) {
      if (this.portType == READ && that.portType == READWRITE) that.we := False
      else that.we                                                     := we
    }
  }

  def <<(that: XILINX_BRAM_PORT) = that >> this

  def doRead(addrIn: UInt) = {
    require(portType != WRITE, "cannot read through write port")
    addr                          := addrIn
    en                            := True
    if (portType == READWRITE) we := False
  }

  def doWrite(addrIn: UInt, data: Bits) = {
    require(portType != READ, "cannot write through read port")
    addr   := addrIn
    dataIn := data
    en     := True
    we     := True
  }

  def simRead(addrIn: BigInt) = {
    require(portType != WRITE, "cannot read through write port")
    addr                          #= addrIn
    en                            #= true
    if (portType == READWRITE) we #= false
  }

  def simWrite(addrIn: BigInt, data: BigInt) = {
    addr   #= addrIn
    en     #= true
    we     #= true
    dataIn #= data
  }

  def preAssign(): Unit = {
    addr.assignDontCare() // the pattern of using don't care as pre-assignment is of great value
    addr.allowOverride
    en.assignDontCare()
    en.allowOverride
    if (portType != READ) {
      dataIn.assignDontCare()
      dataIn.allowOverride
      we.assignDontCare()
      we.allowOverride
    }
  }


}

/** General BRAM model for FPGAs
 * BRAMs are natively synchronous both at read and write port
 */
case class BlockRam(dataWidth: Int, addressWidth: Int) extends Component {

  val ioA = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))
  val ioB = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))

  val bram = Mem(Bits(dataWidth bits), BigInt(1) << addressWidth)

  ioA.dataOut := bram.readWriteSync(ioA.addr, ioA.dataIn, ioA.en, ioA.we)
  ioB.dataOut := bram.readWriteSync(ioB.addr, ioB.dataIn, ioB.en, ioB.we)

  def preAssign() = {
    ioA.preAssign()
    ioB.preAssign()
  }

}

object XILINX_BRAM18E2 {
  def apply() = BlockRam(18, 10)
}

object XILINX_BRAM18E2_PREASSIGNED {
  def apply() = {
    val ret = XILINX_BRAM18E2()
    ret.preAssign()
    ret
  }
}
