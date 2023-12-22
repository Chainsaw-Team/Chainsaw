package Chainsaw.project

import Chainsaw.xillybus.XillybusDevice
import spinal.core._

import scala.language.postfixOps

package object das {

  // das项目的复位配置: 异步,低电平有效
  val dasClockConfig =
    ClockDomainConfig(
      clockEdge        = RISING,
      resetKind        = ASYNC,
      resetActiveLevel = LOW
    )

  // PCIe configuration
  val dasCtrlDevice = XillybusDevice("ctrl", "mem", "write", 32, 16)
  val dasAcqDevice  = XillybusDevice("acq", "fifo", "read", 32)
  val dasDevices    = Seq(dasCtrlDevice, dasAcqDevice)

  val HEADER = BigInt("40c040c0", 16)

  val byte     = HardType(UInt(8 bits))
  val halfWord = HardType(UInt(16 bits))
  val word     = HardType(UInt(32 bits))

  var ctrlClockRate = 125
  var dataWidth     = 8
  var atSimTime     = false
}
