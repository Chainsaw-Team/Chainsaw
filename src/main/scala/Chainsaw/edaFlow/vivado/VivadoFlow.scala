// create project | synth | impl | bitstream through Vivado for your design

package Chainsaw.edaFlow.vivado

import Chainsaw._
import Chainsaw.edaFlow._
import org.apache.commons.io.FileUtils
import org.slf4j._
import spinal.core._
import spinal.core.internals.PhaseContext
import spinal.lib.DoCmd

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

// TODO: using project mode instead of non-project mode

/**
 * @param designInput
 * @param device FPGA chip abstraction
 * @param taskType task that you want to run in Vivado, including SYNTH, IMPL, BITSTREAM, etc.
 * @param optimizeOption
 * @param xdcFile .xdc(Xilinx Design Constraint) file for clock & physical constraint, necessary for BITSTREAM task
 * @param blackBoxSet
 * @param memBinaryFile
 * @tparam T
 */
case class VivadoFlow[T <: Module](
    designInput: ChainsawEdaFlowInput[T],
    device: ChainsawDevice,
    taskType: EdaFlowType,
    optimizeOption: VivadoOptimizeOption,
    xdcFile: Option[File]                    = None,
    blackBoxSet: Option[Set[String]]         = None,
    memBinaryFile: Option[Map[String, File]] = None
) extends EdaFlow(
      designInput.getRtlDir(),
      designInput.workspaceDir,
      designInput.topModuleName,
      device,
      taskType,
      optimizeOption,
      blackBoxSet,
      memBinaryFile
    ) {

  require(hasVivado, "to use VivadoFlow, please set the environment variable 'VIVADO' to the vivado executable, " +
    "e.g. /tools/Xilinx/Vivado/2022.1/bin/vivado")
  require(device.vendor == Xilinx,s"${device.vendor} device is not supported by Vivado Flow")

  val vivadoLogger = LoggerFactory.getLogger(s"VivadoFlow")

  // dirs
  val genScriptDir = new File(designInput.workspaceDir, s"genScript_${designInput.topModuleName}")


  val tclFile       = new File(genScriptDir, "run_vivado.tcl")
  val xdcFileBeUsed = new File(genScriptDir, "vivado_constraint.xdc")
  val logFile       = new File(genScriptDir, "vivado_flow.log")

  override def genScript(): String = {
    vivadoLogger.info(s"Generating tcl script by user configuration...")

    val targetPeriod = device.fMax.toTime
    val clkConstr    = s"""create_clock -period ${(targetPeriod * 1e9) toBigDecimal} [get_ports clk]"""
    val genXdcFile   = new File(genScriptDir, s"generatedXdc.xdc")
    FileUtils.write(genXdcFile, clkConstr)

    val xdcCandidates = Seq(xdcFile, device.xdcFile, Some(genXdcFile))
    xdcFileBeUsed.delete()
    FileUtils.copyFile(xdcCandidates.dropWhile(_.isEmpty).head.get, xdcFileBeUsed)

    var script = ""
    def getReadCommand(sourcePath: File): String = {
      if (sourcePath.getPath.endsWith(".sv")) s"read_verilog -sv $sourcePath \n"
      else if (sourcePath.getPath.endsWith(".v")) s"read_verilog $sourcePath \n"
      else if (sourcePath.getPath.endsWith(".vhdl") || sourcePath.getPath.endsWith(".vhd")) s"read_vhdl $sourcePath \n"
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

    // FIXME: clear directory before run scripts

    // create project
    script += s"create_project ${designInput.topModuleName} ${genScriptDir.getAbsolutePath} -part ${device.familyPart} -force\n"
    script += s"set_property PART ${device.familyPart} [current_project]\n"
    designInput.getRtlDir().foreach(path => script += getReadCommand(path))

    def addReadXdcTask(): Unit = {
      script += s"read_xdc ${xdcFileBeUsed.getAbsolutePath}\n"
    }
    def addSynthTask(activateOOC: Boolean = true): Unit = {
      script += s"synth_design -part ${device.familyPart}  -top ${designInput.topModuleName} ${if (activateOOC) "-mode out_of_context"
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
      script += s"\nwrite_checkpoint -force ${designInput.topModuleName}_after_synth.dcp\n"
      script += s"report_timing\n"
    }

    def addImplTask(): Unit = {
      script += "opt_design\n"
      script += "place_design -directive Explore\n"
      script += "report_timing\n"
      script += s"write_checkpoint -force ${designInput.topModuleName}_after_place.dcp\n"
      script += "phys_opt_design\n"
      script += "report_timing\n"
      script += s"write_checkpoint -force ${designInput.topModuleName}_after_place_phys_opt.dcp\n"
      script += "route_design\n"
      script += s"write_checkpoint -force ${designInput.topModuleName}_after_route.dcp\n"
      script += "report_timing\n"
      script += "phys_opt_design\n"
      script += "report_timing\n"
      script += s"write_checkpoint -force ${designInput.topModuleName}_after_route_phys_opt.dcp\n"
    }

    def addBitStreamTask(): Unit = {
      script += s"write_bitstream -force ${designInput.topModuleName}.bit\n"
    }

    def addReportUtilizationTask(depth: Int = 10): Unit = {
      script += s"report_utilization -hierarchical -hierarchical_depth $depth\n"
    }

    addReadXdcTask()

    val xillybus_pcie_dir = new File(s"/home/ltr/Chainsaw/src/main/resources/ip/xillybus_pcie_ku")
    script +=
      s"read_verilog ${xillybus_pcie_dir}/xillybus.v\n" +
      s"read_verilog ${xillybus_pcie_dir}/xillybus_core.v\n" +
      s"read_edif ${xillybus_pcie_dir}/xillybus_core.edf\n" +
      s"import_ip ${xillybus_pcie_dir}/pcie_ku.xci\n" +
      "report_ip_status\n" +
      "upgrade_ip [get_ips]\n" +
      "synth_ip [get_ips]\n"

    taskType match {
      case PROJECT => // do nothing
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
    VivadoReport(logFile, device)
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
      blackBoxSet: Option[Set[String]],
      memBinaryFile: Option[Map[String, File]]
  ): VivadoFlow[T] = VivadoFlow(
    ChainsawEdaFullInput(design, designDirs, workspaceDir, topModuleName, Some(customizedConfig)),
    device,
    taskType,
    optimizeOption,
    xdcFile,
    blackBoxSet,
    memBinaryFile
  )
}

// usage
object VivadoTask {

  private def inVirtualGlob[T](func: => T): T = {
    val old = GlobalData.get

    val virtualGlob = new GlobalData(SpinalConfig())
    virtualGlob.phaseContext = new PhaseContext(SpinalConfig())
    GlobalData.set(virtualGlob)
    val ret = func

    GlobalData.set(old)
    ret
  }

  def general[T <: Module](
      design: => T,
      name: String,
      device: ChainsawDevice,
      taskType: EdaFlowType,
      includeRtlFile: Seq[File], //
      xdcFile: Option[File],
      customizedConfig: SpinalConfig
  ): VivadoReport = {

    val task = VivadoFlow(
      ChainsawEdaFullInput(design, includeRtlFile, new File(synthWorkspace, name), name, Some(customizedConfig)),
      device,
      taskType,
      VivadoOptimizeOption(),
      xdcFile,
      None,
      None
    )
    val report = task.startFlow()
    report.showInfosReport()
    report
  }

  def synth[T <: Module](
      design: => T,
      name: String,
      device: ChainsawDevice         = vu9p,
      customizedConfig: SpinalConfig = xilinxDefaultSpinalConfig,
      includeFile: Seq[File]         = Seq[File](),
      ignoreBudget: Boolean          = false
  ): VivadoReport = {
    val task = VivadoFlow(
      ChainsawEdaFullInput(design, includeFile, new File(synthWorkspace, name), name, Some(customizedConfig)),
      device,
      SYNTH,
      VivadoOptimizeOption(),
      None,
      None,
      None
    )
    val report = task.startFlow()
    if (!ignoreBudget) {
      report.requireFmax(device.fMax)
      report.requireUtil(device.budget, PreciseRequirement)
    }
    report.showInfosReport()
    report
  }

  def synthModule[T <: Module](
      design: => T,
      name: String,
      device: ChainsawDevice         = vu9p,
      customizedConfig: SpinalConfig = xilinxDefaultSpinalConfig,
      ignoreBudget: Boolean          = false
  ): VivadoReport = {
    val task = VivadoFlow(
      ChainsawEdaModuleInput(design, new File(synthWorkspace, name), name, Some(customizedConfig)),
      device,
      SYNTH,
      VivadoOptimizeOption(),
      None,
      None,
      None
    )
    val report = task.startFlow()
    if (!ignoreBudget) {
      report.requireFmax(device.fMax)
      report.requireUtil(device.budget, PreciseRequirement)
    }
    report.showInfosReport()
    report
  }

  def synthDirs[T <: Module](
      dirs: Seq[File],
      name: String,
      device: ChainsawDevice = vu9p,
      ignoreBudget: Boolean  = false
  ): VivadoReport = {
    val task = VivadoFlow(
      ChainsawEdaDirInput(dirs, new File(synthWorkspace, name), name),
      device,
      SYNTH,
      VivadoOptimizeOption(),
      None,
      None,
      None
    )
    val report = task.startFlow()
    if (!ignoreBudget) {
      report.requireFmax(device.fMax)
      report.requireUtil(device.budget, PreciseRequirement)
    }
    report.showInfosReport()
    report
  }

  def impl[T <: Module](
      design: => T,
      includeFile: Seq[File],
      name: String,
      device: ChainsawDevice         = vu9p,
      customizedConfig: SpinalConfig = xilinxDefaultSpinalConfig,
      ignoreBudget: Boolean          = false
  ): VivadoReport = {
    val task = VivadoFlow(
      ChainsawEdaFullInput(design, includeFile, new File(synthWorkspace, name), name, Some(customizedConfig)),
      device,
      IMPL,
      VivadoOptimizeOption(),
      None,
      None,
      None
    )
    val report = task.startFlow()
    if (!ignoreBudget) {
      report.requireFmax(device.fMax)
      report.requireUtil(device.budget, PreciseRequirement)
    }
    report.showInfosReport()
    report
  }

  def implModule[T <: Module](
      design: => T,
      name: String,
      device: ChainsawDevice         = vu9p,
      customizedConfig: SpinalConfig = xilinxDefaultSpinalConfig,
      ignoreBudget: Boolean          = false
  ): VivadoReport = {
    impl(design, Seq[File](), name, device, customizedConfig, ignoreBudget)
//    val task = VivadoFlow(
//      ChainsawEdaModuleInput(design, new File(synthWorkspace, name), name, Some(customizedConfig)),
//      device,
//      IMPL,
//      VivadoOptimizeOption(),
//      None,
//      None,
//      None
//    )
//    val report = task.startFlow()
//    if (!ignoreBudget) {
//      report.requireFmax(device.fMax)
//      report.requireUtil(device.budget, PreciseRequirement)
//    }
//    report.showInfosReport()
//    report
  }

  def implDirs[T <: Module](
      dirs: Seq[File],
      name: String,
      device: ChainsawDevice = vu9p,
      ignoreBudget: Boolean  = false
  ): VivadoReport = {
    val task = VivadoFlow(
      ChainsawEdaDirInput(dirs, new File(synthWorkspace, name), name),
      device,
      IMPL,
      VivadoOptimizeOption(),
      None,
      None,
      None
    )
    val report = task.startFlow()
    if (!ignoreBudget) {
      report.requireFmax(device.fMax)
      report.requireUtil(device.budget, PreciseRequirement)
    }
    report.showInfosReport()
    report
  }

  def genBitStream[T <: Module](
      design: => T,
      name: String,
      device: ChainsawDevice,
      xdcFile: Option[File]          = None,
      customizedConfig: SpinalConfig = xilinxDefaultSpinalConfig,
      includeFile: Seq[File]         = Seq[File](),
      ignoreBudget: Boolean          = false
  ): VivadoReport = {
    val task = VivadoFlow(
      ChainsawEdaFullInput(design, includeFile, new File(synthWorkspace, name), name, Some(customizedConfig)),
      device,
      BIN,
      VivadoOptimizeOption(),
      xdcFile,
      None,
      None
    )
    val report = task.startFlow()
    if (!ignoreBudget) {
      report.requireFmax(device.fMax)
      report.requireUtil(device.budget, PreciseRequirement)
    }
    report.showInfosReport()
    report
  }

  def genModuleBitStream[T <: Module](
      design: => T,
      name: String,
      device: ChainsawDevice,
      xdcFile: Option[File]          = None,
      customizedConfig: SpinalConfig = xilinxDefaultSpinalConfig,
      ignoreBudget: Boolean          = false
  ): VivadoReport = {


    val task = VivadoFlow(
      ChainsawEdaModuleInput(design, new File(synthWorkspace, name), name, Some(customizedConfig)),
      device,
      BIN,
      VivadoOptimizeOption(),
      xdcFile,
      None,
      None
    )
    val report = task.startFlow()
    if (!ignoreBudget) {
      report.requireFmax(device.fMax)
      report.requireUtil(device.budget, PreciseRequirement)
    }
    report.showInfosReport()
    report
  }

  def genDirBitStream[T <: Module](
      dirs: Seq[File] = Seq[File](),
      name: String,
      device: ChainsawDevice,
      xdcFile: Option[File] = None,
      ignoreBudget: Boolean = false
  ): VivadoReport = {
    val task = VivadoFlow(
      ChainsawEdaDirInput(dirs, new File(synthWorkspace, name), name),
      device,
      BIN,
      VivadoOptimizeOption(),
      xdcFile,
      None,
      None
    )
    val report = task.startFlow()
    if (!ignoreBudget) {
      report.requireFmax(device.fMax)
      report.requireUtil(device.budget, PreciseRequirement)
    }
    report.showInfosReport()
    report
  }



  def genBoardBitStream(
      design: => Module with Board,
      name: String
  ): VivadoReport = {

    val (device, xdcFile) = inVirtualGlob {
      (design.device, design.xdcFile)
    }

    genModuleBitStream(design, name, device, Some(xdcFile))
  }

}

