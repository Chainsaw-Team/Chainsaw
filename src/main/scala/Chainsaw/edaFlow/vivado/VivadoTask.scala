package Chainsaw.edaFlow.vivado

import Chainsaw.edaFlow.Device.{ChainsawDevice, vu9p}
import Chainsaw.edaFlow._
import Chainsaw.{BIN, EdaFlowType, IMPL, PROJECT, SYNTH, synthWorkspace}
import spinal.core.{Module, SpinalConfig}

import java.io.File

/** Entrances of VivadoFlow, for use case, see unit test of [[VivadoFlow]]
  */
object VivadoTask {

  val defaultXilinxDevice = vu9p

  def general(
      name: String,
      design: => Module,
      taskType: EdaFlowType,
      device: ChainsawDevice         = null,
      includeRtlFile: Seq[File]      = Seq[File](),
      xdcFile: Option[File]          = None,
      customizedConfig: SpinalConfig = xilinxDefaultSpinalConfig
  ): VivadoReport = {
    // construct edaInput
    val edaInput: ChainsawEdaFlowInput = inVirtualGlob(
      if (design == null) ChainsawEdaDirInput(includeRtlFile, new File(synthWorkspace, name), name)
      else if (includeRtlFile.nonEmpty)
        ChainsawEdaFullInput(design, includeRtlFile, new File(synthWorkspace, name), name, Some(customizedConfig))
      else ChainsawEdaModuleInput(design, new File(synthWorkspace, name), name, Some(customizedConfig))
    )
    // init task
    val task = VivadoFlow(
      edaInput,
      device,
      taskType,
      VivadoOptimizeOption(),
      xdcFile,
      None
    )
    // run task and return report
    val report = task.startFlow()
    report.showInfosReport()
    report
  }

  // synthesis
  def synth: (String, => Module, ChainsawDevice, Seq[File], Option[File], SpinalConfig) => VivadoReport =
    general(_, _, SYNTH, _, _, _, _)
  def synthModuleOn: (String, => Module, ChainsawDevice) => VivadoReport =
    synth(_, _, _, Seq[File](), None, xilinxDefaultSpinalConfig)
  def synthModule: (String, => Module) => VivadoReport = synthModuleOn(_, _, defaultXilinxDevice)
  def synthNetlistOn: (String, ChainsawDevice, Seq[File]) => VivadoReport =
    synth(_, null, _, _, None, xilinxDefaultSpinalConfig)
  def synthNetlist: (String, Seq[File]) => VivadoReport = synthNetlistOn(_, defaultXilinxDevice, _)

  // implementation
  def impl: (String, => Module, ChainsawDevice, Seq[File], Option[File], SpinalConfig) => VivadoReport =
    general(_, _, IMPL, _, _, _, _)
  def implModuleOn: (String, => Module, ChainsawDevice) => VivadoReport =
    impl(_, _, _, Seq[File](), None, xilinxDefaultSpinalConfig)

  def implModule: (String, => Module) => VivadoReport = implModuleOn(_, _, defaultXilinxDevice)

  def implNetlistOn: (String, ChainsawDevice, Seq[File]) => VivadoReport =
    impl(_, null, _, _, None, xilinxDefaultSpinalConfig)

  def implNetlist: (String, Seq[File]) => VivadoReport = implNetlistOn(_, defaultXilinxDevice, _)

  // bitstream generation

  def genBitStream: (String, => Module, ChainsawDevice, Seq[File], Option[File], SpinalConfig) => VivadoReport =
    general(_, _, BIN, _, _, _, _)

  def genBitStreamForBoard: (String, => Module with Board) => VivadoReport =
    genBitStream(_, _, null, Seq[File](), None, xilinxDefaultSpinalConfig)

  // project creation
  def createProject = general(_, _, PROJECT, null, Seq[File](), None, xilinxDefaultSpinalConfig)

}
