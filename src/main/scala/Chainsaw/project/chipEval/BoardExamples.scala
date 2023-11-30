package Chainsaw.project.chipEval

import Chainsaw.DataUtil
import Chainsaw.edaFlow.boards.{AN9767, AXKU041, FL1010}
import Chainsaw.edaFlow.vivado.VivadoTask
import spinal.core._
import spinal.lib._

case class DaExample() extends AXKU041 {

  defaultClockDomain on {

    useFmc2(asMaster = true, dataOnly = true)

    val an9767 = AN9767()

    val sinValues = (0 until 1000).map(i => (Math.sin(i * 2 * Math.PI / 1000) * ((1 << 13) - 1)).toInt + (1 << 13))
    val sinROM: Mem[UInt] = Mem(sinValues.map(U(_, 14 bits)))

    val clkCounter = CounterFreeRun(100)
    val daClk      = (clkCounter.value >= U(49, 7 bits)).d()

    val addrCounter = Counter(1000, inc = daClk.rise())
    val da2Clk = RegInit(False)
    da2Clk.toggleWhen(daClk.rise())

//    val data = sinROM.readSync(addrCounter.value).d() // FIXME: import .bin file

    val data        = RegInit(U(0, 14 bits))
    when(da2Clk.rise())(data := AN9767.getVoltageValue(1.8))
    when(da2Clk.fall())(data := AN9767.getVoltageValue(0.0))

    // manual trigger -> DAC
    val trigger = user_key.d(3).rise()
    val pulseGen = Timeout(5 us)
    when(trigger)(pulseGen.clear())

    when(~pulseGen.state)(data := AN9767.getVoltageValue(1.8))
      .otherwise(data := AN9767.getVoltageValue(0.0))

    // ROM -> AN9767
    Seq(an9767.channel1, an9767.channel2).foreach(_                                               := data)
    Seq(an9767.channel1Clk, an9767.channel2Clk, an9767.channel1Wrt, an9767.channel2Wrt).foreach(_ := daClk)
    // ALINX40PIN -> FMC-LPC
    FMC2_LPC <> FL1010(an9767.alinx40PinOut, an9767.alinx40PinOut)

    // refClk -> SMA
    SMA_CLKIN_P := daClk
    SMA_CLKIN_N := !daClk
  }

}

object DaExample extends App {

  SpinalVerilog(DaExample())
  VivadoTask.genBoardBitStream(DaExample(), "DaExample")

}
