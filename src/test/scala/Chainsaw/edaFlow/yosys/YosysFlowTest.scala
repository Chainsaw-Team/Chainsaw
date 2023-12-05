package Chainsaw.edaFlow.yosys

import Chainsaw._
import Chainsaw.edaFlow._
import Chainsaw.edaFlow.Device._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._

import java.io.File
object TestYosysUtils {
  def testYosysFlow(
      design: => Component,
      optimizeOption: YosysOptimizeOption,
      customizedConfig: Option[SpinalConfig] = None,
      includeDirs: Seq[File]                 = Seq[File](),
      workspaceDir: File                     = new File(synthWorkspace, "Yosys"),
      topModuleName: String                  = " ",
      device: ChainsawDevice                 = zcu104,
      blackBoxSet: Option[Set[String]]       = None
  ) = {
    YosysFlow(
      designInput    = new ChainsawEdaFullInput(design, includeDirs, workspaceDir, topModuleName, customizedConfig),
      optimizeOption = optimizeOption,
      device         = device,
      blackBoxSet    = blackBoxSet
    )
      .startFlow()
  }
}

class YosysFlowTest extends AnyFunSuite {

  test("test xilinx device for yosys flow") {
    TestYosysUtils.testYosysFlow(
      new StreamFifo(UInt(16 bits), 32),
      YosysXilinxOptimizeOption(),
      topModuleName = "StreamFifo"
    )
  }

  test("test xilinx device for yosys flow with blackbox") {
    TestYosysUtils.testYosysFlow(
      new StreamFifo(UInt(16 bits), 32),
      YosysXilinxOptimizeOption(),
      blackBoxSet   = Some(Set("StreamFifo")),
      topModuleName = "StreamFifo"
    )
  }

  test("test general device for yosys flow") {
    TestYosysUtils.testYosysFlow(
      new StreamFifo(UInt(16 bits), 32),
      YosysGeneralOptimizeOption(),
      device        = generic,
      topModuleName = "StreamFifo"
    )

  }
}
