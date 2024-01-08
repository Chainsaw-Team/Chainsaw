package Chainsaw.edaFlow.vivado

import Chainsaw._
import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow._
import org.apache.commons.io.FileUtils
import org.slf4j._
import spinal.core._
import spinal.lib.DoCmd

import java.io.File
import scala.io.Source

/** Abstraction of a Vivado Flow
  * @param designInput
  *   the vivadoFlow design source, it may be directories containing source code|component|both
  * @param device
  *   FPGA chip abstracted by [[ChainsawDevice]], when the designInput contains a [[Board]], this parameter will be
  *   ignored
  * @param taskType
  *   specify the task type (Project creation, Simulation, Synthesis, Implementation, BitStream Generation)
  * @param optimizeOption
  *   specify the optimize option will be used in vivadoFlow
  * @param xdcFile
  *   specify the xilinx design constraint file will be used in [[VivadoFlow]], when the designInput contains a
  *   [[Board]], this parameter will be ignored
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
    "to use VivadoFlow, please set the environment variable 'VIVADO' to the vivado executable, " +
      "e.g. /tools/Xilinx/Vivado/2022.1/bin/vivado"
  )

  // directory & file location
  val genScriptDir = new File(designInput.getWorkspaceDir(), s"genScript_${designInput.getTopModuleName()}")
  val tclFile = new File(genScriptDir, "run_vivado.tcl")
  private val xdcFileInProject = new File(genScriptDir, "vivado_constraint.xdc")
  val logFile = new File(genScriptDir, "vivado_flow.log")

  // decide actual device & xdcFile in use
  private val deviceInUse = designInput.getDevice(device)
  private val xdcFileInUse = designInput.getXdcFile(xdcFile.getOrElse { // priority: boardXdcFile > xdcFile > genXdcFile
    val generatedXdcFile = new File(genScriptDir, s"generatedXdc.xdc")
    val targetPeriod     = deviceInUse.fMax.toTime
    val clkConstr        = s"""create_clock -period ${(targetPeriod * 1e9) toBigDecimal} [get_ports clk]"""
    FileUtils.write(generatedXdcFile, clkConstr)
    generatedXdcFile
  })
  FileUtils.copyFile(xdcFileInUse, xdcFileInProject)

  deviceInUse.vendor match {
    case Xilinx =>
    case _ =>
      throw new IllegalArgumentException(
        s"The device type must be of Xilinx family."
      )
  }

  val vivadoLogger = LoggerFactory.getLogger(s"VivadoFlow")



  override def genScript(): String = {
    vivadoLogger.info(s"Generating tcl script by user configuration...")

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
    script += s"create_project ${designInput.getTopModuleName()} ${genScriptDir.getAbsolutePath} -part ${deviceInUse.familyPart} -force\n"
    script += s"set_property PART ${deviceInUse.familyPart} [current_project]\n"

    // reading RTL sources
    designInput.getRtlDir().foreach(path => script += getReadCommand(path))

    def addReadXdcTask(): Unit = {
      script += s"read_xdc ${xdcFileInProject.getAbsolutePath}\n"
    }
    def addSynthTask(activateOOC: Boolean = true): Unit = {
      script += s"synth_design -part ${deviceInUse.familyPart}  -top ${designInput.getTopModuleName()} ${if (activateOOC) "-mode out_of_context"
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
        addReportUtilizationTask()
      case IMPL =>
        addSynthTask()
        addImplTask()
        addReportUtilizationTask()
      case BIN =>
        addSynthTask(activateOOC = false)
        addImplTask()
        addBitStreamTask()
        addReportUtilizationTask()
    }

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
    VivadoReport(logFile, deviceInUse)
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
