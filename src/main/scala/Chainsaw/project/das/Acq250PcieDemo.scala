package Chainsaw.project.das

import Chainsaw.edaFlow.PythonHeaderGenerator
import Chainsaw.xillybus.{XillybusBusIf, XillybusWrapper}
import spinal.core._
import spinal.lib.bus.regif.AccessType

case class AcqPcieDemo() extends Acq250 {

  setDefinitionName("Acq250Top")

  xillybus.pcieClockDomain on {
    // loopback
    download_32.queue(32) >> upload_32
    download_8.queue(32)  >> upload_8

    // controller
    val memBusIf      = XillybusBusIf(mem_32)
    val pulseGenReg   = memBusIf.newReg("pulseGen")
    val pulseGenField = pulseGenReg.field(UInt(8 bits), AccessType.RW, 0, "pulse generation flag")
    memBusIf.accept(PythonHeaderGenerator("Acq250", "Acq250"))
  }
}

object AcqPcieDemo extends App {

  SpinalVerilog(AcqPcieDemo())

}
