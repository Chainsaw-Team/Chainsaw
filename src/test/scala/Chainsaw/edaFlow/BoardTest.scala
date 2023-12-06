package Chainsaw.edaFlow

import Chainsaw.{BIN, DataUtil}
import Chainsaw.edaFlow.boards.digilent.Nexys4Ddr
import Chainsaw.edaFlow.vivado.VivadoTask
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File

class BoardTest extends AnyFlatSpec {

  case class Nexys4Led() extends Nexys4Ddr {
    defaultClockDomain on {
      LED := SW.d(2)
    }
  }



  it should "synthesis board with its device and xdcFile" in {
    VivadoTask.general("Nexys4Led", Nexys4Led(), BIN, null, Seq[File](), None, xilinxDefaultSpinalConfig)
  }
}
