package Chainsaw.project.das

import Chainsaw.edaFlow.PythonHeaderGenerator
import Chainsaw.xillybus.{XillybusBusIf, XillybusFifoRead, XillybusFifoWrite, XillybusMemBi, XillybusWrapper}
import spinal.core._
import spinal.lib._

case class Acq250Top() extends Acq250 {

  val devices = Seq(
    XillybusFifoRead("read_32", 32),
    XillybusFifoWrite("write_32", 32, 8), // unused, set a shallow FIFO
    XillybusFifoRead("read_8", 8, 8),     // unused, set a shallow FIFO
    XillybusFifoWrite("write_8", 8, 8),   // unused, set a shallow FIFO
    XillybusMemBi("mem_32", 32, 16)
  )

  val xillybus = XillybusWrapper(4, devices, device, Some(dataClockDomain))
  xillybus.pcieIntel <> pcie
  val pcieClockDomain = xillybus.pcieClockDomain

  val upload_32   = xillybus.getStreamToHost("read_32")
  val download_32 = xillybus.getStreamFromHost("write_32")
  val upload_8    = xillybus.getStreamToHost("read_8")
  val download_8  = xillybus.getStreamFromHost("write_8")
  val mem_32      = xillybus.memBus

  // parameters
  val word250M           = 1.toLong << 31
  val mhz_number         = word250M / 250
  def freqWord(mhz: Int) = mhz * mhz_number // get frequency control word

  ////////////////////
  // PCIe area for register file and DDS control
  ////////////////////

  val lvdsDebug = LVDSDEBUG()
  lvdsDebug.adc_clk   := adc_clk
  lvdsDebug.rstn      := rstn
  lvdsDebug.adcBundle := adcBundle

  val lvdsClockDomain = ClockDomain(
    clock  = lvdsDebug.lvds_clk,
    reset  = rstn,
    config = ClockDomainConfig(resetActiveLevel = LOW)
  )

  val pcieArea = new ClockingArea(pcieClockDomain) {

    val memBusIf = XillybusBusIf(mem_32)

    // loopback
    download_8 >> upload_8
    download_32.ready.set()

    // register file

    // DDS control
    val ddsCtrl = new Ad9959Ctrl(
      Ad9959Config(
        frequencyDividerRatio = RegConfig(getFdr()),
        freq0                 = RegConfig(freqWord(100)), // for carrier 1
        freq1                 = RegConfig(freqWord(80)),  // for carrier 2
        freq2                 = RegConfig(word250M),      // for adc
        freq3                 = RegConfig(word250M / 2),  // for data processing & pulse generation
        phase2                = ConstantConfig(0),
        phase3                = ConstantConfig(0)
      ),
      Some(memBusIf), // mount DDS on memBus
      ddsBundle
    )

    // pulse generation

  }

  ////////////////////
  // data area for pulse generation and data processing
  ////////////////////

  val dataArea = new ClockingArea(dataClockDomain) {
    val pulseCtrl = PulseCtrl(pcieArea.memBusIf, pcieClockDomain, pulseBundle, gain)
    val dataOut   = Stream(Bits(32 bits))
    val dataPath = DataPath(
      busIf           = pcieArea.memBusIf,
      busClockDomain  = pcieClockDomain,
      lvdsClockDomain = lvdsClockDomain,
      lvdsDataIn      = lvdsDebug.adcData,
      pulseRise       = pulseCtrl.pulseRise,
      gpsInfo         = B(0, 32 bits),
      dataOut         = dataOut
    )

  }

  dataArea.dataOut >> upload_32
  pcieArea.memBusIf.accept(PythonHeaderGenerator("Acq250", "Acq250")) // generate python header

}

object Acq250Top extends App {

  SpinalVerilog(Acq250Top())

}
