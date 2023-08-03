package Chainsaw.edaFlow.xilinx

import Chainsaw._
import Chainsaw.arithmetic.Cpa
import Chainsaw.edaFlow._
import Chainsaw.edaFlow.vivado._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._

import java.io.File
object TestVivadoUtils {
  def testVivadoFlow(
      design: => Component,
      optimizeOption: VivadoOptimizeOption,
      customizedConfig: Option[SpinalConfig] = None,
      includeDirs: Option[Seq[File]]         = None,
      workspaceDir: File                     = new File(synthWorkspace, "Vivado"),
      topModuleName: String,
      deviceType: edaFlow.Device       = zcu104,
      taskType: EdaFlowType            = SYNTH,
      xdcFile: Option[File]            = None,
      blackBoxSet: Option[Set[String]] = None
  ) = {
    VivadoFlow
      .fromComponent(
        design           = design,
        customizedConfig = customizedConfig,
        includeDirs      = includeDirs,
        workspaceDir     = workspaceDir,
        topModuleName    = topModuleName,
        optimizeOption   = optimizeOption,
        deviceType       = deviceType,
        taskType         = taskType,
        xdcFile          = xdcFile,
        blackBoxSet      = blackBoxSet
      )
      .startFlow()
  }
}

class VivadoFlowTest extends AnyFunSuite {

  test("test xilinx device for vivado synth flow") {
    TestVivadoUtils.testVivadoFlow(
      new StreamFifo(UInt(16 bits), 32),
      VivadoOptimizeOption(),
      topModuleName = "StreamFifo"
    )
  }

  test("test xilinx device for vivado synth flow with blackbox") {
    TestVivadoUtils.testVivadoFlow(
      Cpa(BinaryAdder, 16).implH,
      VivadoOptimizeOption(),
      topModuleName = "unamed_1",
      blackBoxSet   = Some(Set("unamed"))
    )
  }

  test("test xilinx device for vivado impl flow") {
    TestVivadoUtils.testVivadoFlow(
      new StreamFifo(UInt(16 bits), 32),
      VivadoOptimizeOption(),
      topModuleName = "StreamFifo",
      taskType      = IMPL
    )
  }
}
