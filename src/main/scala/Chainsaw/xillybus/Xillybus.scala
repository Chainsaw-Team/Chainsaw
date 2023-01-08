package Chainsaw.xillybus

import spinal.core._

import scala.language.postfixOps

/** black box generator for xillybus IP, configured by a list of devices
 *
 * @param devices the device files you define through xillybus IP factory [[http://xillybus.com/custom-ip-factory]]
 */
case class Xillybus(devices: Seq[XillybusDevice]) extends BlackBox {
  val quiesce, bus_clk = out Bool()
  val user_led = out Bits (4 bits)
  val pcie = PcieBundle()

  val streamsRead = devices.filter(device => device.direction == "read" && device.deviceType == "fifo")
  val streamsWrite = devices.filter(device => device.direction == "write" && device.deviceType == "fifo")
  val memsWrite = devices.filter(device => device.direction == "write" && device.deviceType == "mem")

  val streamReadInterfaces = streamsRead.map(StreamRead)
  val streamWriteInterfaces = streamsWrite.map(StreamWrite)
  val memWriteInterfaces = memsWrite.map(MemWrite)

  setDefinitionName("xillybus")
}

/** --------
 * interfaces of different kinds of devices supported by xillybus
 *
 * @see [[http://xillybus.com/custom-ip-factory]]
 *      -------- */

case class StreamRead(device: XillybusDevice)
  extends Bundle {
  val rden, open = out Bool()
  val empty, eof = in Bool()
  val data = in Bits (device.bitWidth bits)
  this.setName(s"${device.fullName}")
}

case class StreamWrite(device: XillybusDevice)
  extends Bundle {
  val wren, open = out Bool()
  val full = in Bool()
  val data = out Bits (device.bitWidth bits)
  this.setName(s"${device.fullName}")
}

case class MemBi(device: XillybusDevice) extends Bundle {
  // Host -> FPGA
  val wren, openW = out Bool()
  val dataW = out Bits (device.bitWidth bits)
  val full = in Bool()
  val wName = s"user_w_${device.name}"
  wren.setName(s"${wName}_wren")
  full.setName(s"${wName}_full")
  dataW.setName(s"${wName}_data")
  openW.setName(s"${wName}_open")
  // FPGA -> Host
  val rden, openR = out Bool()
  val empty, eof = in Bool()
  val dataR = in Bits (device.bitWidth bits)
  val rName = s"user_r_${device.name}"
  rden.setName(s"${rName}_rden")
  empty.setName(s"${rName}_empty")
  dataR.setName(s"${rName}_data")
  eof.setName(s"${rName}_eof")
  openR.setName(s"${rName}_open")
  // addr
  val addr = out UInt (device.addrWidth bits)
  val addr_update = out Bool()
  addr.setName(s"user_${device.name}_addr")
  addr_update.setName(s"user_${device.name}_addr_update")
}

case class MemWrite(device: XillybusDevice) extends Bundle {
  val wren, open, addr_update = out Bool()
  val addr = out UInt (device.addrWidth bits)
  val data = out UInt (device.bitWidth bits)
  val full = in Bool()
  this.setName(s"${device.fullName}")
  addr.setName(s"user_${device.name}_addr")
  addr_update.setName(s"user_${device.name}_addr_update")
}