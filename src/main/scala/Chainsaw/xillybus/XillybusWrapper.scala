package Chainsaw.xillybus

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** pcie-related signals on a board, which are necessary for a xillybus IP
  */
case class PcieBundle() extends Bundle {
  val perstn, refclk = in Bool ()
  val rx             = in Bits (4 bits)
  val tx             = out Bits (4 bits)
  this.setName("pcie")
}

/** interface of a MemWrite device when you use it a as control channel which
  * manipulate a register file
  *
  * @param ctrlDevice
  *   the MemWrite device configuration
  */
case class XillybusCtrl(ctrlDevice: XillybusDevice) extends Bundle {
  val update = Bool()
  val addr   = UInt(ctrlDevice.addrWidth bits)
  val value  = UInt(ctrlDevice.bitWidth bits)
}

case class FifoWriteInterface(width: Int) extends Bundle with IMasterSlave {
  val wrreq, wrfull = Bool()
  val data          = Bits(width bits)

  override def asMaster(): Unit = {
    in(wrfull)
    out(wrreq, data)
  }
}

case class FifoReadInterface(width: Int) extends Bundle with IMasterSlave {
  val rdreq, rdempty = Bool()
  val q              = Bits(width bits)

  override def asMaster(): Unit = {
    in(rdempty, q)
    out(rdreq)
  }
}

/** wrapper for xillybus that generate FIFOs for each stream channel, and expose
  * the ctrl interface for the memory-map channel
  *
  * @param devices
  *   the device files you define through xillybus IP factory
  *   [[http://xillybus.com/custom-ip-factory]]
  */
case class XillybusWrapper(devices: Seq[XillybusDevice]) extends Component {

  val dataClk =
    in Bool () // clock from user application domain(rather than xillybus domain)
  val ctrlClk = in Bool () // same as above
  val pcie    = PcieBundle()

  val xillybus = Xillybus(devices) // xillybus IP

  // I/O
  val fifoWriteInterfaces = xillybus.streamsRead.map(device =>
    slave(FifoWriteInterface(device.bitWidth))
  )
  val fifoReadInterfaces = xillybus.streamsWrite.map(device =>
    slave(FifoReadInterface(device.bitWidth))
  )
  require(
    xillybus.memsWrite.length == 1,
    "this wrapper is valid only when xillybus has one and only one MemWrite device as control interface"
  )
  val ctrlDevice = xillybus.memsWrite.head
  val ctrlOut    = out(XillybusCtrl(ctrlDevice))

  // global connections
  pcie <> xillybus.pcie

  val dataDomain = ClockDomain(
    clock  = dataClk,
    reset  = xillybus.pcie.perstn,
    config = ClockDomainConfig(resetActiveLevel = LOW)
  )
  val ctrlDomain = ClockDomain(
    clock  = ctrlClk,
    reset  = xillybus.pcie.perstn,
    config = ClockDomainConfig(resetActiveLevel = LOW)
  )
  val xillyDomain = ClockDomain(
    clock  = xillybus.bus_clk,
    reset  = xillybus.pcie.perstn,
    config = ClockDomainConfig(resetActiveLevel = LOW)
  )

  val bufferSize = 4096

  // stream channels connections
  xillybus.streamsRead
    .zip(xillybus.streamReadInterfaces.zip(fifoWriteInterfaces))
    .foreach { case (device, (busSide, userSide)) =>
      val dcFifo = StreamFifoCC(
        Bits(device.bitWidth bits),
        bufferSize / (device.bitWidth / 8),
        dataDomain,
        xillyDomain
      )
      dcFifo.setName(s"${device.name}_fifo")
      // xillybus <-> FIFO
      dcFifo.io.pop.ready := busSide.rden
      busSide.empty       := !dcFifo.io.pop.valid
      busSide.data        := dcFifo.io.pop.payload
      busSide.eof         := False
      // user logic <-> FIFO
      dcFifo.io.push.valid   := userSide.wrreq
      dcFifo.io.push.payload := userSide.data
      userSide.wrfull        := !dcFifo.io.push.ready

      dcFifo.io.pushOccupancy.setName("pushOccupancy")
      dcFifo.io.pop.setName("popOccupancy")
    }

  xillybus.streamsWrite
    .zip(xillybus.streamWriteInterfaces.zip(fifoReadInterfaces))
    .foreach { case (device, (busSide, userSide)) =>
      val dcFifo = StreamFifoCC(
        Bits(device.bitWidth bits),
        bufferSize.toInt / (device.bitWidth / 8),
        xillyDomain,
        dataDomain
      )
      dcFifo.setName(s"${device.name}_fifo")
      // xillybus <-> FIFO
      dcFifo.io.push.valid   := busSide.wren
      dcFifo.io.push.payload := busSide.data
      busSide.full           := !dcFifo.io.push.ready
      // user logic <-> FIFO
      dcFifo.io.pop.ready := userSide.rdreq
      userSide.rdempty    := !dcFifo.io.pop.valid
      busSide.data        := dcFifo.io.pop.payload
    }

  // ctrl channel connections
  val ctrlChannel = xillybus.memWriteInterfaces.head
  ctrlChannel.full := False // as we use a register file

  val combinedType = HardType(
    UInt(ctrlDevice.bitWidth + ctrlDevice.addrWidth bits)
  )
  val ctrlFifo = StreamFifoCC(combinedType, 16, xillyDomain, ctrlDomain)
  ctrlFifo.setName(s"${ctrlDevice.name}_fifo")
  ctrlFifo.io.push.valid   := ctrlChannel.wren
  ctrlFifo.io.push.payload := ctrlChannel.addr @@ ctrlChannel.data

  ctrlOut.update := ctrlFifo.io.pop.valid
  ctrlOut.addr  := ctrlFifo.io.pop.payload.takeHigh(ctrlDevice.addrWidth).asUInt
  ctrlOut.value := ctrlFifo.io.pop.payload.takeLow(ctrlDevice.bitWidth).asUInt
  ctrlFifo.io.pop.ready := True

  def getReadInterfaceByName(name: String) =
    xillybus.streamsWrite
      .zip(fifoReadInterfaces)
      .find(_._1.name == name)
      .get
      ._2

  def getWriteInterfaceByName(name: String) =
    xillybus.streamsRead
      .zip(fifoWriteInterfaces)
      .find(_._1.name == name)
      .get
      ._2
}
