package Chainsaw.edaFlow.vivado

import Chainsaw._
import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow._
import org.apache.commons.io.FileUtils
import org.slf4j._
import spinal.core._
import spinal.lib.DoCmd

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/** The class which can be used to run VivadoTask
  * @param designInput
  *   the vivadoFlow design source, it can be file dir format or Component format or mixed input of dir and Component
  * @param device
  *   the device which will be used in edaFlow to synthesis or implementation
  * @param taskType
  *   specify the task type (Simulation, Synthesis, Implementation, Generate Binary)
  * @param optimizeOption
  *   specify the optimize option will be used in vivadoFlow
  * @param xdcFile
  *   specify the xilinx design constraint file will be used in [[VivadoFlow]], if your design is Board, this file will
  *   invalid
  * @param blackBoxSet
  *   specify which module will be viewed as BlackBox(will change v or sv file)
  * @tparam T
  *   the class of Component or its subClass
  */
case class VivadoFlow[T <: Module](
    designInput: ChainsawEdaFlowInput,
    device: ChainsawDevice,
    taskType: EdaFlowType,
    optimizeOption: VivadoOptimizeOption,
    xdcFile: Option[File]            = None,
    blackBoxSet: Option[Set[String]] = None
) extends EdaFlow(
      designInput.getRtlDir(),
      designInput.getWorkspaceDir(),
      designInput.getTopModuleName(),
      designInput.getDevice(device),
      taskType,
      optimizeOption,
      blackBoxSet
    ) {

  require(
    hasVivado,
    "to use VivadoFlow, please set the environment variable 'VIVADO' to the vivado executable, e.g. /tools/Xilinx/Vivado/2022.1/bin/vivado"
  )

  val usedDevice = designInput.getDevice(device)

  usedDevice.vendor match {
    case Xilinx =>
    case _ =>
      throw new IllegalArgumentException(
        s"The device type must be Xilinx family."
      )
  }
  val vivadoLogger = LoggerFactory.getLogger(s"VivadoFlow")

  val genScriptDir = new File(designInput.getWorkspaceDir(), s"genScript_${designInput.getTopModuleName()}")

  val tclFile       = new File(genScriptDir, "run_vivado.tcl")
  val xdcFileBeUsed = new File(genScriptDir, "vivado_constraint.xdc")
  val logFile       = new File(genScriptDir, "vivado_flow.log")

  override def genScript(): String = {
    vivadoLogger.info(s"Generating tcl script by user configuration...")

    val targetPeriod = usedDevice.fMax.toTime
    val clkConstr    = s"""create_clock -period ${(targetPeriod * 1e9) toBigDecimal} [get_ports clk]"""
    val genXdcFile   = new File(genScriptDir, s"generatedXdc.xdc")
    FileUtils.write(genXdcFile, clkConstr)

    val candidateXdcFile = Seq(xdcFile, Some(genXdcFile)).dropWhile(_.isEmpty).head.get
    val boardXdcFile     = designInput.getXdcFile(candidateXdcFile)
    xdcFileBeUsed.delete()
    FileUtils.copyFile(boardXdcFile, xdcFileBeUsed)

    var script = ""
    def getReadCommand(sourcePath: File): String = {
      if (sourcePath.getPath.endsWith(".sv")) s"read_verilog -sv $sourcePath \n"
      else if (sourcePath.getPath.endsWith(".v")) s"read_verilog $sourcePath \n"
      else if (
        sourcePath.getPath
          .endsWith(".vhdl") || sourcePath.getPath.endsWith(".vhd")
      ) s"read_vhdl $sourcePath \n"
      else if (sourcePath.getPath.endsWith(".bin")) "\n"
      else
        throw new IllegalArgumentException(
          s"invalid RTL source path $sourcePath"
        )
    }

    if (blackBoxSet.isDefined) blackBoxSet.get.foreach { module =>
      val regex =
        s"\\s*module\\s*($module\\s+.*)"
      designInput.getRtlDir().foreach { dir =>
        val files = if (dir.isDirectory) dir.listFiles().toSeq else Seq(dir)
        files.foreach { file =>
          val fileClass      = new File(file.getAbsolutePath)
          val contents       = Source.fromFile(fileClass)
          var trans_contents = ""
          contents.getLines().toSeq.foreach { line =>
            val matched = regex.r.findFirstMatchIn(line)
            if (matched.isDefined) trans_contents += s"(* black_box *) module ${matched.get.group(1)}\n"
            else trans_contents += s"$line\n"
          }
          fileClass.delete()
          FileUtils.write(fileClass, trans_contents)
        }
      }
    }

    designInput.getRtlDir().foreach(path => script += getReadCommand(path))

    def addReadXdcTask(): Unit = {
      script += s"read_xdc ${xdcFileBeUsed.getAbsolutePath}\n"
    }
    def addSynthTask(activateOOC: Boolean = true): Unit = {
      script += s"synth_design -part ${usedDevice.familyPart}  -top ${designInput.getTopModuleName()} ${if (activateOOC) "-mode out_of_context"
      else ""}\t"
      if (optimizeOption.isFlatten) script += s"-flatten_hierarchy full\t"
      if (optimizeOption.convertClkGateLogic) script += s"-gated_clock_conversion on\t"
      if (optimizeOption.maxclkBuf != -1) script += s"-bufg ${optimizeOption.maxclkBuf}\t"
      if (!optimizeOption.enableLutComb) script += s"-no_lc\t"
      if (optimizeOption.maxFanout != -1) script += s"-fanout_limit ${optimizeOption.maxFanout}\t"
      if (optimizeOption.fsmRecode) script += s"-fsm_extraction one_hot\t"
      if (optimizeOption.keepEquReg) script += s"-keep_equivalent_registers\t"
      if (optimizeOption.resourceShare) script += s"-resource_sharing on\t"
      if (optimizeOption.reTiming) script += s"-retiming\t"
      if (optimizeOption.maxBram != -1) script += s"-max_bram ${optimizeOption.maxBram}\t"
      if (optimizeOption.maxUram != -1) script += s"-max_uram ${optimizeOption.maxUram}\t"
      if (optimizeOption.maxDsp != -1) script += s"-max_dsp ${optimizeOption.maxDsp}\t"
      script += s"\nwrite_checkpoint -force ${designInput.getTopModuleName()}_after_synth.dcp\n"
      script += s"report_timing\n"
    }

    def addImplTask(): Unit = {
      script += "opt_design\n"
      script += "place_design -directive Explore\n"
      script += "report_timing\n"
      script += s"write_checkpoint -force ${designInput.getTopModuleName()}_after_place.dcp\n"
      script += "phys_opt_design\n"
      script += "report_timing\n"
      script += s"write_checkpoint -force ${designInput.getTopModuleName()}_after_place_phys_opt.dcp\n"
      script += "route_design\n"
      script += s"write_checkpoint -force ${designInput.getTopModuleName()}_after_route.dcp\n"
      script += "report_timing\n"
      script += "phys_opt_design\n"
      script += "report_timing\n"
      script += s"write_checkpoint -force ${designInput.getTopModuleName()}_after_route_phys_opt.dcp\n"
    }

    def addBitStreamTask(): Unit = {
      script += s"write_bitstream -force ${designInput.getTopModuleName()}.bit\n"
    }

    def addReportUtilizationTask(depth: Int = 10): Unit = {
      script += s"report_utilization -hierarchical -hierarchical_depth $depth\n"
    }

    addReadXdcTask()
    taskType match {
      case SYNTH =>
        addSynthTask()
      case IMPL =>
        addSynthTask()
        addImplTask()
      case BIN =>
        addSynthTask(activateOOC = false)
        addImplTask()
        addBitStreamTask()
    }
    addReportUtilizationTask()
    FileUtils.write(tclFile, script)
    vivadoLogger.info(s"Finish Vivado script generation...")
    script
  }

  override def runScript(): Unit = {
    vivadoLogger.info(s"Running VivadoFlow...")
    // run vivado
    DoCmd.doCmd(
      s"${VIVADO.path} -stack 2000 -nojournal -log ${logFile.getAbsolutePath} -mode batch -source ${tclFile.getAbsolutePath}",
      genScriptDir.getAbsolutePath
    )
    vivadoLogger.info(s"Finish VivadoFlow...")
  }

  def startFlow(): VivadoReport = {
    genScript()
    runScript()
    VivadoReport(logFile, usedDevice)
  }
}

object VivadoFlow {
  def apply[T <: Module](
      design: => T,
      designDirs: Seq[File],
      workspaceDir: File,
      topModuleName: String,
      customizedConfig: SpinalConfig,
      device: ChainsawDevice,
      taskType: EdaFlowType,
      optimizeOption: VivadoOptimizeOption,
      xdcFile: Option[File],
      blackBoxSet: Option[Set[String]]
  ): VivadoFlow[T] = VivadoFlow(
    ChainsawEdaFullInput(design, designDirs, workspaceDir, topModuleName, Some(customizedConfig)),
    device,
    taskType,
    optimizeOption,
    xdcFile,
    blackBoxSet
  )
}

object VivadoTask {
  def general[T <: Module](
      name: String,
      design: => T,
      device: ChainsawDevice,
      taskType: EdaFlowType,
      includeRtlFile: Seq[File],
      xdcFile: Option[File],
      customizedConfig: SpinalConfig
  ): VivadoReport = {
    val edaInput = inVirtualGlob(
      if (design == null) ChainsawEdaDirInput(includeRtlFile, new File(synthWorkspace, name), name)
      else if (includeRtlFile.nonEmpty)
        ChainsawEdaFullInput(design, includeRtlFile, new File(synthWorkspace, name), name, Some(customizedConfig))
      else ChainsawEdaModuleInput(design, new File(synthWorkspace, name), name, Some(customizedConfig))
    )

    val task = VivadoFlow(
      edaInput,
      device,
      taskType,
      VivadoOptimizeOption(),
      xdcFile,
      None
    )
    val report = task.startFlow()
    report.showInfosReport()
    report
  }

  def synth: (String, => Module, ChainsawDevice, Seq[File], Option[File], SpinalConfig) => VivadoReport = {
    general(_, _, _, SYNTH, _, _, _)
  }

  def fastSynth: (String, => Module, Seq[File]) => VivadoReport =
    general(_, _, vu9p, SYNTH, _, None, xilinxDefaultSpinalConfig)

  def synthModule: (String, => Module, ChainsawDevice, Option[File], SpinalConfig) => VivadoReport =
    synth(_, _, _, Seq[File](), _, _)

  def fastSynthModule: (String, => Module) => VivadoReport =
    synth(_, _, vu9p, Seq[File](), None, xilinxDefaultSpinalConfig)

  def synthDirs: (String, ChainsawDevice, Seq[File], Option[File], SpinalConfig) => VivadoReport =
    synth(_, null, _, _, _, _)

  def fastSynthDirs: (String, Seq[File]) => VivadoReport = synthDirs(_, vu9p, _, None, xilinxDefaultSpinalConfig)

  def impl: (String, => Module, ChainsawDevice, Seq[File], Option[File], SpinalConfig) => VivadoReport =
    general(_, _, _, IMPL, _, _, _)

  def fastImpl: (String, => Module, Seq[File]) => VivadoReport =
    general(_, _, vu9p, IMPL, _, None, xilinxDefaultSpinalConfig)

  def implModule: (String, => Module, ChainsawDevice, Option[File], SpinalConfig) => VivadoReport =
    impl(_, _, _, Seq[File](), _, _)

  def fastImplModule: (String, => Module) => VivadoReport =
    impl(_, _, vu9p, Seq[File](), None, xilinxDefaultSpinalConfig)

  def implDirs: (String, ChainsawDevice, Seq[File], Option[File], SpinalConfig) => VivadoReport =
    impl(_, null, _, _, _, _)

  def fastImplDirs: (String, Seq[File]) => VivadoReport =
    impl(_, null, vu9p, _, None, xilinxDefaultSpinalConfig)

  def genBitStream: (String, => Module, ChainsawDevice, Seq[File], Option[File], SpinalConfig) => VivadoReport =
    general(_, _, _, BIN, _, _, _)

  def genModuleBitStream: (String, => Module, ChainsawDevice, Option[File], SpinalConfig) => VivadoReport =
    genBitStream(_, _, _, Seq[File](), _, _)

  def fastGenModuleBitStream: (String, => Module) => VivadoReport =
    genModuleBitStream(_, _, vu9p, None, xilinxDefaultSpinalConfig)

  def genDirBitStream: (String, ChainsawDevice, Seq[File], Option[File], SpinalConfig) => VivadoReport =
    genBitStream(_, null, _, _, _, _)

  def fastGenDirBitStream: (String, Seq[File]) => VivadoReport =
    genDirBitStream(_, vu9p, _, None, xilinxDefaultSpinalConfig)

//  def genBoardBitStream(
//      design: => Module with Board,
//      name: String
//  ): VivadoReport = {
//
//    val (device, xdcFile) = inVirtualGlob {
//      (design.device, design.xdcFile)
//    }
//
//    genModuleBitStream(design, name, vu9p, None, xilinxDefaultSpinalConfig)
////    genModuleBitStream(design, name, design.device, Some(design.xdcFile))
//  }
}
