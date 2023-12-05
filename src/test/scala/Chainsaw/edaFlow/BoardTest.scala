package Chainsaw.edaFlow

import Chainsaw.edaFlow.Device.vu9p
import Chainsaw.edaFlow.vivado.VivadoTask
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File

class BoardTest extends AnyFlatSpec {
  it should "synthesis board with its device and xdcFile" in {
//    VivadoTask.synthModule("Nexys4Led", Nexys4Led(), vu9p, None, xilinxDefaultSpinalConfig)
//    VivadoTask.fastSynthModule("Nexys4Led", Nexys4Led())
    VivadoTask.fastGenModuleBitStream("Nexys4Led", Nexys4Led())
  }
}
