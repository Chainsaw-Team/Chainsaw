package Chainsaw.project

import Chainsaw.edaFlow.Device.{AlteraDevice, CycloneV}
import Chainsaw.xillybus.XillybusDevice
import Chainsaw._
import spinal.core._

import scala.language.postfixOps

/** Package object `das` provides configuration settings and constants for the DAS project.
  */
package object das {

  //
  val dasClockConfig =
    ClockDomainConfig(
      clockEdge        = RISING,
      resetKind        = ASYNC,
      resetActiveLevel = LOW
    )
  val dasDevice = new AlteraDevice(CycloneV, "5CGXFC9D6F27I7", 125 MHz, None) // FPGA we use

  // PCIe configuration
  val dasCtrlDevice      = XillybusDevice("ctrl", "mem", "write", 32, 16)
  val dasAcqDevice       = XillybusDevice("acq", "fifo", "read", 32)
  val dasXillybusDevices = Seq(dasCtrlDevice, dasAcqDevice)

  val HEADER = BigInt("40c040c0", 16)

  // alias
  val byte     = HardType(UInt(8 bits))
  val halfWord = HardType(UInt(16 bits))
  val word     = HardType(UInt(32 bits))

  // compilation settings,may change for different boards

  var ctrlClockRate = 125

  def getFdr(): Int = {
    println(s"frequencyDividerRatio = ${500 / ctrlClockRate}")
    500 / ctrlClockRate
  }

  var dataWidth = 8
  var atSimTime = false

  // utils
  def getControlData[T <: Data](ctrl: T) = {
    ctrl.addTag(crossClockDomain)
    ctrl.d(3)
  }
}
