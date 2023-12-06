package Chainsaw.examples

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.regif._

import scala.language.postfixOps

// defining a new bus interface
class YourBus(dataWidth: Int, addrWidth: Int) extends Bundle with IMasterSlave {
  val wren, rden, error = Bool()
  val wData, rData      = Bits(dataWidth bits)
  val addr              = UInt(addrWidth bits)

  override def asMaster(): Unit = {
    out(wren, rden, wData, addr)
    in(rData, error)
  }
}

object YourBus {
  def apply(dataWidth: Int, addrWidth: Int): YourBus = new YourBus(dataWidth, addrWidth)
}

/** Bus interface for Xillybus mem interface
  */
case class YourBusIf(bus: YourBus) extends BusIf {

  override type B = this.type
  override def getModuleName = "YourBus"

  override val regPre = "ctrl"

  override val askWrite = False // no usage in SpinalHDL
  override val doWrite =
    bus.wren // write request, valid when wData is valid, inner register value changed 1 cycle after
  override val writeData = bus.wData // data to write

  override val askRead = bus.rden     // read request
  override val doRead  = bus.rden.d() // valid when readData is valid, 1 cycle after rden
//  override val doRead    = False // TODO: seems it has no effect on readData, only on read_hit
  override val readData = RegInit(B(0, bus.rData.getBitsWidth bits)) // data read, must be a register
  override val readError = RegInit(False) // data read error(out of range, .etc), must be a register

  bus.rData := readData
  bus.error := readError

  override def readAddress() = bus.addr

  override def writeAddress() = bus.addr

  override def readHalt(): Unit = {}

  override def writeHalt(): Unit = {}

  override def busDataWidth = bus.rData.getBitsWidth

}

// using bus interface to generate a register file in your design

case class YourController() extends Module {

  val busIn        = slave(YourBus(dataWidth = 8, addrWidth = 5)) // regFile containing 32 8-bit register
  val busInterface = YourBusIf(busIn)

  val value0 = busInterface.newReg("value0").field(UInt(8 bits), AccessType.RW, 0, "value0")
  val value1 = busInterface.newReg("value1").field(UInt(8 bits), AccessType.RW, 0, "value1")

  busInterface.accept(HtmlGenerator("yourBus", "yourBus"))
  busInterface.accept(CHeaderGenerator("yourBus", "yourBus"))

  out(value0, value1)
}

// simulation
object YourController extends App {

  SimConfig.withFstWave.compile(YourController()).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()

    dut.busIn.rden  #= false
    dut.busIn.wren  #= false
    dut.busIn.addr  #= 0
    dut.busIn.wData #= 0

    dut.clockDomain.waitSampling()

    dut.busIn.rden  #= false
    dut.busIn.wren  #= true
    dut.busIn.addr  #= 0
    dut.busIn.wData #= 0x10

    dut.clockDomain.waitSampling()

    dut.busIn.rden  #= false
    dut.busIn.wren  #= true
    dut.busIn.addr  #= 1
    dut.busIn.wData #= 0x11

    dut.clockDomain.waitSampling()

    dut.busIn.rden  #= true
    dut.busIn.wren  #= false
    dut.busIn.addr  #= 0
    dut.busIn.wData #= 0

    dut.clockDomain.waitSampling()

    dut.busIn.rden  #= true
    dut.busIn.wren  #= false
    dut.busIn.addr  #= 1
    dut.busIn.wData #= 0

    dut.clockDomain.waitSampling()

    dut.busIn.rden  #= true
    dut.busIn.wren  #= false
    dut.busIn.addr  #= 2 // addr out of range lead to readError
    dut.busIn.wData #= 0

    dut.clockDomain.waitSampling(10)

  }

}
