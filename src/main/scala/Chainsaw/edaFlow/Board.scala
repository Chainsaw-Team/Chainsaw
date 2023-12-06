package Chainsaw.edaFlow

import Chainsaw.edaFlow.Device.ChainsawDevice
import spinal.core._

import java.io.File

trait Board {
  val xdcFile: File
  val device: ChainsawDevice
  val defaultClockDomain: ClockDomain
}
