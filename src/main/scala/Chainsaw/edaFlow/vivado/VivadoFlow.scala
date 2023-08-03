package Chainsaw.edaFlow.vivado

import Chainsaw._
import Chainsaw.edaFlow._
import org.apache.commons.io.FileUtils
import org.slf4j._
import spinal.core._
import spinal.lib.DoCmd

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class VivadoFlow(
    designDirs: Seq[File],
    workspaceDir: File,
    topModuleName: String,
    deviceType: edaFlow.Device,
    taskType: EdaFlowType,
    optimizeOption: VivadoOptimizeOption,
    xdcFile: Option[File]                    = None,
    blackBoxSet: Option[Set[String]]         = None,
    memBinaryFile: Option[Map[String, File]] = None
) extends EdaFlow(
      designDirs,
      workspaceDir,
      Some(topModuleName),
      Some(deviceType),
      taskType,
      optimizeOption,
      blackBoxSet,
      memBinaryFile
    ) {

  require(
    hasVivado,
    "to use VivadoFlow, please set the environment variable 'VIVADO' to the vivado executable, e.g. /tools/Xilinx/Vivado/2022.1/bin/vivado"
  )

  deviceType.vendor match {
    case Xilinx =>
    case _ =>
      throw new IllegalArgumentException(
        s"The device type must be Xilinx family."
      )
  }
  val vivadoLogger = LoggerFactory.getLogger(s"VivadoFlow")

  val genScriptDir = EdaDirectoryUtils.genTargetWorkspace(target = "Script", Some(topModuleName), workspaceDir)

  val tclFile       = new File(genScriptDir, "run_vivado.tcl")
  val simpleXdcFile = new File(genScriptDir, "vivado_constraint.xdc")
  val logFile       = new File(genScriptDir, "vivado_flow.log")

  override def genScript(): String = {
    vivadoLogger.info(s"Generating tcl script by user configuration...")
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
      designDirs.foreach { dir =>
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

    designDirs.foreach(path => script += getReadCommand(path))

    taskType match {
      case SYNTH =>
        script += s"synth_design -part ${deviceType.familyPart}  -top $topModuleName -mode out_of_context  "
        if (xdcFile.isDefined) script += s"-constrset ${xdcFile.get.getAbsolutePath}  "
        if (optimizeOption.isFlatten) script += s"-flatten_hierarchy full "
        if (optimizeOption.convertClkGateLogic) script += s"-gated_clock_conversion on  "
        if (optimizeOption.maxclkBuf != -1) script += s"-bufg ${optimizeOption.maxclkBuf}  "
        if (!optimizeOption.enableLutComb) script += s"-no_lc  "
        if (optimizeOption.maxFanout != -1) script += s"-fanout_limit ${optimizeOption.maxFanout}  "
        if (optimizeOption.fsmRecode) script += s"-fsm_extraction one_hot "
        if (optimizeOption.keepEquReg) script += s"-keep_equivalent_registers  "
        if (optimizeOption.resourceShare) script += s"-resource_sharing on  "
        if (optimizeOption.reTiming) script += s"-retiming  "
        if (optimizeOption.maxBram != -1) script += s"-max_bram ${optimizeOption.maxBram}  "
        if (optimizeOption.maxUram != -1) script += s"-max_uram ${optimizeOption.maxUram}  "
        if (optimizeOption.maxDsp != -1) script += s"-max_dsp ${optimizeOption.maxDsp}"
        script += s"\n"
        script += s"write_checkpoint -force ${topModuleName}_after_synth.dcp \n"
        script += s"report_timing \n"
      case IMPL =>
        script += "opt_design \n"
        script += "place_design -directive Explore \n"
        script += "report_timing \n"
        script += s"write_checkpoint -force ${topModuleName}_after_place.dcp \n"
        script += "phys_opt_design \n"
        script += "report_timing \n"
        script += s"write_checkpoint -force ${topModuleName}_after_place_phys_opt.dcp \n"
        script += "route_design \n"
        script += s"write_checkpoint -force ${topModuleName}_after_route.dcp \n"
        script += "report_timing \n"
        script += "phys_opt_design \n"
        script += "report_timing \n"
        script += s"write_checkpoint -force ${topModuleName}_after_route_phys_opt.dcp \n"
    }

    script += s"report_utilization -hierarchical -hierarchical_depth 10 \n"
    FileUtils.write(tclFile, script)
    vivadoLogger.info(s"Finish Vivado script generation...")
    script
  }

  override def runScript(): Unit = {
    vivadoLogger.info(s"Running VivadoFlow...")
    // run vivado
    DoCmd.doCmd(
      s"${vivadoPath.getAbsolutePath} -stack 2000 -nojournal -log ${logFile.getAbsolutePath} -mode batch -source ${tclFile.getAbsolutePath}",
      genScriptDir.getAbsolutePath
    )
    vivadoLogger.info(s"Finish VivadoFlow...")
  }

  override def startFlow(): Unit = {
    genScript()
    runScript()
    VivadoReport(logFile, deviceType).genInfosReport()
  }
}

object VivadoFlow {
  def fromComponent(
      design: => Component,
      customizedConfig: Option[SpinalConfig]   = None,
      includeDirs: Option[Seq[File]]           = None,
      workspaceDir: File                       = new File(synthWorkspace, "Vivado"),
      topModuleName: String                    = "StreamFifo",
      deviceType: edaFlow.Device               = zcu104,
      taskType: EdaFlowType                    = SYNTH,
      optimizeOption: VivadoOptimizeOption     = VivadoOptimizeOption(),
      xdcFile: Option[File]                    = None,
      blackBoxSet: Option[Set[String]]         = None,
      memBinaryFile: Option[Map[String, File]] = None
  ): VivadoFlow = {

    val rtlResources: ArrayBuffer[File] = ArrayBuffer()
    val supportedFileTypes              = Seq(".v", ".sv", ".vhdl")

    if (includeDirs.isDefined) {
      includeDirs.get.foreach { dir =>
        rtlResources ++= dir
          .listFiles()
          .toSeq
          .map(_.getAbsolutePath)
          .filter(path => supportedFileTypes.exists(fileType => path endsWith fileType))
          .map(new File(_))
      }
    }

    rtlResources ++= EdaDirectoryUtils.genRtlSourcesFromComponent(
      design,
      customizedConfig,
      Some(topModuleName),
      workspaceDir,
      supportedFileTypes
    )

    VivadoFlow(
      rtlResources,
      workspaceDir,
      topModuleName,
      deviceType,
      taskType,
      optimizeOption,
      xdcFile,
      blackBoxSet,
      memBinaryFile
    )

  }
}
