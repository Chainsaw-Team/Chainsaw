package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axis._
import spinal.lib.bus.amba4.axilite._

import scala.language.postfixOps

object AxiExample extends App {

  /** --------
   * AXI-Stream example
   * -------- */
  case class AxisFifoExample() extends Component {

    val byteCount = 8

    val config = Axi4StreamConfig(dataWidth = byteCount)

    val clkIn, clkOut = in Bool()
    val areset = in Bool()
    val dataIn = slave(Axi4Stream(config))
    val dataOut = master(Axi4Stream(config))

    val domainIn = ClockDomain(clock = clkIn, reset = areset)
    val domainOut = ClockDomain(clock = clkOut, reset = areset)

    val streamFifo = StreamFifoCC(Bits(byteCount * 8 bits), 128, domainIn, domainOut)

    dataIn.toBitStream() >> streamFifo.io.push // AxiStream -> Stream
    Axi4Stream(streamFifo.io.pop) >> dataOut // Stream -> AxiStream
  }

  SpinalConfig().generateVerilog(AxisFifoExample())

  case class AxisWidthAdapterExample() extends Component {

    val byteCountIn = 4
    val byteCountOut = 8

    val configIn = Axi4StreamConfig(dataWidth = byteCountIn)
    val configOut = Axi4StreamConfig(dataWidth = byteCountOut)

    val dataIn = slave(Axi4Stream(configIn))
    val dataOut = master(Axi4Stream(configOut))

    val streamAdapter = Axi4StreamSimpleWidthAdapter(dataIn, dataOut)
  }

  SpinalConfig().generateVerilog(AxisWidthAdapterExample())

  /** --------
   * AXI-Lite example
   * -------- */

  case class AxiLiteExample() extends Component {

    val config = AxiLite4Config(5, 32)

    val ctrlIn = slave(AxiLite4(config))
    val ctrlOut = master(AxiLite4(config))

    ctrlIn >> ctrlOut
  }

  SpinalConfig().generateVerilog(AxiLiteExample())
}