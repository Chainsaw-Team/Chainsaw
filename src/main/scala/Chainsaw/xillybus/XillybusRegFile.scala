package Chainsaw.xillybus

import spinal.core._
import spinal.lib.bus.regif.BusIf

import scala.language.postfixOps

/** a simple bus for host -> device control by register file
 *
 * @param ctrlDevice xillybus device of the control interface
 */
case class XillybusRegFile(ctrlDevice: XillybusDevice) extends Bundle {
  // host -> xillybus
  val ctrlIn = in(XillybusCtrl(ctrlDevice))
  // xilllybus -> user logic
  val readAddr = in UInt (ctrlDevice.addrWidth bits)
  val readData = out Bits (ctrlDevice.bitWidth bits)
  val readError = out Bool()
}

case class XillybusRegIf(bus: XillybusRegFile) extends BusIf {

  import bus._

  override type B = this.type

  require(isPow2(ctrlDevice.bitWidth))
  val extraAddrWidth = log2Up(ctrlDevice.bitWidth / 8)

  override def getModuleName = ctrlDevice.name

  override val regPre = "ctrl"

  override val askWrite = ctrlIn.update
  override val doWrite = ctrlIn.update
  override val writeData = ctrlIn.value.asBits

  override val askRead = True
  override val doRead = True

  override val readData = RegInit(B(0, ctrlDevice.bitWidth bits))
  bus.readData := readData

  override val readError = RegInit(False)
  bus.readError := readError

  override def readAddress() = bus.readAddr

  override def writeAddress() = ctrlIn.addr << extraAddrWidth

  override def readHalt(): Unit = {}

  override def writeHalt(): Unit = {}

  override def busDataWidth = ctrlDevice.bitWidth

}
