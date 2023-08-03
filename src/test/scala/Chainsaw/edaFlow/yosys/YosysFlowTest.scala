package Chainsaw.edaFlow.yosys

import Chainsaw._
import Chainsaw.edaFlow._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._

import java.io.File
object TestYosysUtils {
  def testYosysFlow(
      design: => Component,
      optimizeOption: YosysOptimizeOption,
      customizedConfig: Option[SpinalConfig] = None,
      includeDirs: Option[Seq[File]]         = None,
      workspaceDir: File                     = new File(synthWorkspace, "Yosys"),
      topModuleName: String                  = " ",
      deviceType: Option[edaFlow.Device]     = Some(zcu104),
      blackBoxSet: Option[Set[String]]       = None
  ) = {
    YosysFlow
      .fromComponent(
        design           = design,
        customizedConfig = customizedConfig,
        includeDirs      = includeDirs,
        workspaceDir     = workspaceDir,
        topModuleName    = if (topModuleName != " ") Some(topModuleName) else None,
        optimizeOption   = optimizeOption,
        deviceType       = deviceType,
        blackBoxSet      = blackBoxSet
      )
      .startFlow()
  }
}

class YosysFlowTest extends AnyFunSuite {

  test("test xilinx device for yosys flow") {
    TestYosysUtils.testYosysFlow(
      new StreamFifo(UInt(16 bits), 32),
      YosysXilinxOptimizeOption()
    )
  }

  test("test xilinx device for yosys flow with blackbox") {
    TestYosysUtils.testYosysFlow(
      new StreamFifo(UInt(16 bits), 32),
      YosysXilinxOptimizeOption(),
      blackBoxSet = Some(Set("StreamFifo"))
    )
  }

  test("test general device for yosys flow") {
    TestYosysUtils.testYosysFlow(
      new StreamFifo(UInt(16 bits), 32),
      YosysGeneralOptimizeOption(),
      deviceType = None
    )

  }
}
