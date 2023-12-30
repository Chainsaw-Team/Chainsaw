package Chainsaw.project.ChipVerify

import Chainsaw.DataUtil
import Chainsaw.edaFlow.boards.alinx._
import Chainsaw.xillybus._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.AccessType

import scala.language.postfixOps

case class ChipVerifyZu() extends Z7P {

  defaultClockDomain on {

    // I/O
    out(alinx40Pin)

    // pulse generation logic
    val an9767     = AN9767()
    val clkCounter = CounterFreeRun(100)
    val daClk      = (clkCounter.value >= U(49, 7 bits)).d()

    val da2Clk = RegInit(False)
    da2Clk.toggleWhen(daClk.rise())

    val data = RegInit(U(0, 14 bits))
    when(da2Clk.rise())(data := AN9767.getVoltageValue(1.8))
    when(da2Clk.fall())(data := AN9767.getVoltageValue(0.0))

    // manual trigger -> DAC
    val trigger  = (!pl_key_n).d(3).rise()
    val pulseGen = Timeout(5 us)
    when(trigger)(pulseGen.clear())

    when(~pulseGen.state)(data := AN9767.getVoltageValue(1.8))
      .otherwise(data := AN9767.getVoltageValue(0.0))

    // pulse -> AN9767 -> ALINX40PIN
    Seq(an9767.channel1, an9767.channel2).foreach(_                                               := data)
    Seq(an9767.channel1Clk, an9767.channel2Clk, an9767.channel1Wrt, an9767.channel2Wrt).foreach(_ := daClk)
    alinx40Pin := an9767.alinx40PinOut

    // PCIE logic
    val xillybus = XillybusWrapper.defaultWrapper(pinCount = 4, this.device)
    xillybus.pcieXilinx <> pcie

    // stream loopback
    val upload_32 = xillybus.getStreamToHost("read_32")
    upload_32.addAttribute("mark_debug", "true")
    val download_32 = xillybus.getStreamFromHost("write_32")
    val upload_8    = xillybus.getStreamToHost("read_8")
    val download_8  = xillybus.getStreamFromHost("write_8")

    val Seq(fifo_32, fifo_8) = Seq(32, 8).map(bitWidth =>
      StreamFifoCC(
        dataType = Bits(bitWidth bits),
        depth    = 1024,
        xillybus.pcieClockDomain,
        xillybus.pcieClockDomain
      )
    )
    fifo_32.setName("fifo_32")
    fifo_8.setName("fifo_8")

    download_32    >> fifo_32.io.push
    fifo_32.io.pop >> upload_32
    download_8     >> fifo_8.io.push
    fifo_8.io.pop  >> upload_8

    // mem - register file
    val memBusIf      = XillybusBusIf(xillybus.memBus)
    val pulseGenReg   = memBusIf.newReg("pulseGen")
    val pulseGenField = pulseGenReg.field(UInt(8 bits), AccessType.RW, 0, "pulse generation flag")

  }

}
