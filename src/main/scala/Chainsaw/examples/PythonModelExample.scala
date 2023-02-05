package Chainsaw.examples

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._ // for finite state machine dialect
import spinal.lib.bus._ // for all kinds of bus and regIf
import spinal.lib.bus.regif._ // for regIf
import spinal.sim._ // for simulation
import spinal.core.sim._ // for more simulation

import Chainsaw._ // for basic templates
import Chainsaw.dsp._ // for dsp operators
import Chainsaw.arithmetic._ // for arithmetic operators
import Chainsaw.crypto._ // for crypto operators
import Chainsaw.xilinx._ // for Xilinx FPGA Flow

object PythonModelExample extends App {

  Runtime.getRuntime().exec("python ")

}
