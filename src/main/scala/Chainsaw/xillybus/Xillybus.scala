package Chainsaw.xillybus

import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow.boards.{PcieIntel, PcieXilinx}
import spinal.core._
import spinal.lib.slave

import scala.language.postfixOps

/** black box generator for xillybus IP, configured by a list of devices
  *
  * @param devices
  *   the device files you define through xillybus IP factory [[http://xillybus.com/custom-ip-factory]]
  */
case class Xillybus(pinCount: Int, devices: Seq[XillybusDevice], target: ChainsawDevice) extends BlackBox {
  val quiesce, bus_clk = out Bool ()
  val user_led         = out Bits (4 bits)

  val pcieIntel  = target.isInstanceOf[AlteraDevice].generate(slave(PcieIntel(pinCount)))
  val pcieXilinx = target.isInstanceOf[XilinxDevice].generate(slave(PcieXilinx(pinCount)))

  val streamsRead  = devices.filter(device => device.direction == "read" && device.deviceType == "fifo")
  val streamsWrite = devices.filter(device => device.direction == "write" && device.deviceType == "fifo")
  val memsWrite    = devices.filter(device => device.direction == "write" && device.deviceType == "mem")
  val memsRead     = devices.filter(device => device.direction == "read" && device.deviceType == "mem")
  val memsBi       = devices.filter(device => device.direction == "bi" && device.deviceType == "mem")

  val streamReadInterfaces  = streamsRead.map(StreamRead)
  val streamWriteInterfaces = streamsWrite.map(StreamWrite)
  val memWriteInterfaces    = memsWrite.map(MemBi)
  val memReadInterfaces     = memsRead.map(MemBi)
  val memBiInterfaces       = memsBi.map(MemBi)

  target match {
    case device: AlteraDevice =>
    case device: XilinxDevice =>
      user_led.setName("GPIO_LED")
  }

  setDefinitionName("xillybus")

  addRTLPath("xillybus.v")
  addRTLPath("xillybus_core.v")
  addRTLPath("xillybus_core.edf")
}

/** -------- interfaces of different kinds of devices supported by xillybus
  *
  * @see
  *   [[http://xillybus.com/custom-ip-factory]]
  * --------
  */

case class StreamRead(device: XillybusDevice) extends Bundle {
  val rden, open = out Bool ()
  val empty, eof = in Bool ()
  val data       = in Bits (device.bitWidth bits)
  this.setName(s"${device.fullName}")
}

case class StreamWrite(device: XillybusDevice) extends Bundle {
  val wren, open = out Bool ()
  val full       = in Bool ()
  val data       = out Bits (device.bitWidth bits)
  this.setName(s"${device.fullName}")
}

case class MemBi(device: XillybusDevice) extends Bundle {
  val hasWrite = device.direction == "write" || device.direction == "bi"
  val hasRead  = device.direction == "read" || device.direction == "bi"
  val wName    = s"user_w_${device.name}"
  val rName    = s"user_r_${device.name}"
  println(s"has read = $hasRead, has write = $hasWrite")

  // I/O
  // Host -> FPGA
  val wren, openW = hasWrite generate out Bool ()
  val dataW       = hasWrite generate out Bits (device.bitWidth bits) // should be valid 1 cycle after wren
  val full        = hasWrite generate in Bool ()
  // FPGA -> Host
  val rden, openR = hasRead generate out Bool ()
  val empty, eof  = hasRead generate in Bool ()
  val dataR       = hasRead generate in Bits (device.bitWidth bits) // should be valid 1 cycle after rden
  // addr
  val addr        = out UInt (device.addrWidth bits)
  val addr_update = out Bool ()

  // set names
  if (hasWrite) {
    wren.setName(s"${wName}_wren")
    full.setName(s"${wName}_full")
    dataW.setName(s"${wName}_data")
    openW.setName(s"${wName}_open")
  }
  if (hasRead) {
    rden.setName(s"${rName}_rden")
    empty.setName(s"${rName}_empty")
    dataR.setName(s"${rName}_data")
    eof.setName(s"${rName}_eof")
    openR.setName(s"${rName}_open")
  }
  addr.setName(s"user_${device.name}_addr")
  addr_update.setName(s"user_${device.name}_addr_update")
}
