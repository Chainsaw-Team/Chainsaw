package Chainsaw.xillybus

import Chainsaw._
import spinal.core._
import spinal.lib.bus.regif.BusIf

import scala.language.postfixOps

/** Bus interface for Xillybus mem interface
 */
case class XillybusBusIf(bus: MemBi) extends BusIf {

  override type B = this.type

  require(isPow2(bus.device.bitWidth))
  val extraAddrWidth = log2Up(bus.device.bitWidth / 8)

  override def getModuleName = bus.device.name

  override val regPre = "ctrl"

  override val askWrite  = False
  override val doWrite   = bus.wren
  override val writeData = bus.dataW

  override val askRead   = bus.rden
  override val doRead    = bus.rden.d()
  override val readData  = RegInit(B(0, bus.device.bitWidth bits))
  override val readError = RegInit(False) // unused

  bus.dataR := readData
  bus.full  := False
  bus.empty := False
  bus.eof   := False

  override def readAddress() = bus.addr << extraAddrWidth

  override def writeAddress() = bus.addr << extraAddrWidth

  override def readHalt(): Unit = {}

  override def writeHalt(): Unit = {}

  override def busDataWidth = bus.device.bitWidth

}
