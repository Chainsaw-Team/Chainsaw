package Chainsaw.edaFlow.xilinx

import Chainsaw._
import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow.boards.digilent.Nexys4Ddr
import Chainsaw.edaFlow.vivado._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import java.io.File
import scala.language.postfixOps

case class ThirdPartyDesign() extends Component {
  val dataIn  = in Bits (16 bits)
  val dataOut = out Bits (16 bits)
  dataOut := (~dataIn).d(2)
}

case class YourBlackBox() extends BlackBox {
  val dataIn  = in Bits (16 bits)
  val dataOut = out Bits (16 bits)
//  addRTLPath("ThirdPartyDesign.v")
}

case class YourDesign() extends Component {
  val sw  = in Bits (16 bits)
  val led = out Bits (16 bits)
  led := sw.d(2)
}

case class YourBoard() extends Nexys4Ddr {
  val design = YourDesign()
  design.sw  <> SW
  design.led <> LED
}

case class YourBoardContainingIP() extends Nexys4Ddr {
  val design = YourDesign()
  val ip = YourBlackBox()
  ip.dataIn <> SW
  design.sw  <> ip.dataOut
  design.led <> LED
}

class VivadoFlowTest extends AnyFlatSpec {

  SpinalVerilog(ThirdPartyDesign())
  val someNetList = new File(s"./")

  // designer

  "VivadoFlow" should "help designer to evaluate it's own performance on the fly" in VivadoTask.synthModule(
    "YourDesign",
    YourDesign()
  )

  it should "help designer to evaluate IP(provided in netlist)" in VivadoTask.synthNetlist(
    "ThirdPartyDesign",
    Seq(someNetList)
  )

  it should "help designer to a design on specific chip" in VivadoTask.synthModuleOn("YourDesign", YourDesign(), a7100t)

  // researchers whose main concern is the performance of the design

  it should "help researcher to compare its design with others | on different chips after routing" in {
    val report0 = VivadoTask.synthModuleOn("YourDesign", YourDesign(), a7100t) // own design on a chip
    val report1 = VivadoTask.synthModuleOn("YourDesign", YourDesign(), vu9p) // own design on another chip
    val report2 = VivadoTask.synthNetlistOn("ThirdPartyDesign", a7100t, Seq(someNetList))

    // TODO: methods that compare reports
  }

  // developers who wanna implement the design on specific board
  it should "help developer to create a project containing necessary sources" in VivadoTask.createProject("YourBoard", YourBoard())

  it should "help developer to generate bitstream" in VivadoTask.genBitStreamForBoard("YourBoard", YourBoard())

  // for more complicated situation, using VivadoTask.general or just create a Vivado project and run the flow manually
}
