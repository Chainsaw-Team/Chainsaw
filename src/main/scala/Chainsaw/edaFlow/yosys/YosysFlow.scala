package Chainsaw.edaFlow.yosys

import Chainsaw._
import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow._
import org.apache.commons.io.FileUtils
import org.slf4j._
import spinal.core._
import spinal.lib.DoCmd

import java.io.File

/** The class which can be used to run YosysTask
  * @param designInput
  *   the YosysFlow design source, it can be file dir format or Component format or mixed input of dir and Component
  * @param device
  *   the device which will be used in edaFlow to synthesis or implementation
  * @param optimizeOption
  *   specify the optimize option will be used in edaFlow
  * @param blackBoxSet
  *   specify which module will be viewed as BlackBox(will change v or sv file)
  * @tparam T
  *   the class of Component or its subClass
  */
case class YosysFlow[T <: Module](
    designInput: ChainsawEdaFlowInput,
    device: ChainsawDevice,
    optimizeOption: YosysOptimizeOption,
    blackBoxSet: Option[Set[String]] = None
) extends EdaFlow(
      designInput.getRtlDir(),
      designInput.getWorkspaceDir(),
      designInput.getTopModuleName(),
      device,
      SYNTH,
      optimizeOption,
      blackBoxSet
    ) {

  require(
    YOSYS.exist(),
    "to use YosysFlow, please set the environment variable 'YOSYS' to the Yosys executable, e.g. /opt/Yosys/oss-cad-suite/bin"
  )

  val yosysLogger = LoggerFactory.getLogger(s"YosysFlow")

  val genScriptDir = new File(designInput.getWorkspaceDir(), s"genScript_${designInput.getTopModuleName()}")

  if (genScriptDir.exists()) genScriptDir.delete()

  val logFile     = new File(genScriptDir, "yosys_flow.log")
  val shellFile   = new File(genScriptDir, "run_yosys.sh")
  val yosysScript = new File(genScriptDir, "yosys_script.ys")

  def genYosysScript(): String = {
    yosysLogger.info(s"Generating Yosys script by user configuration...")

    def getReadCommand(sourcePath: File): String = {
      if (sourcePath.getPath.endsWith(".v")) s"read_verilog $sourcePath\n"
      else
        throw new IllegalArgumentException(
          s"invalid RTL source path $sourcePath"
        )
    }

    var script = ""
    designInput.getRtlDir().foreach(file => script += getReadCommand(file))

    if (blackBoxSet.isDefined) blackBoxSet.get.foreach(module => script += s"blackbox $module\n")

    device match {
      case generic: GenericDevice =>
        script += s"synth  -top ${designInput.getTopModuleName()}  "
        optimizeOption match {
          case yosysOption: YosysGeneralOptimizeOption =>
            if (yosysOption.isFlatten) script += s"-flatten  "
            if (!yosysOption.fsmOptimization) script += s"-nofsm  "
            script += s"-lut ${yosysOption.lutArchitecture}  "
            if (yosysOption.keepOpDirectForm) script += s"-noalumacc  "
            if (!yosysOption.absorbMemDff) script += s"-nordff  "
            if (yosysOption.dontCareRWCollision) script += s"-no-rw-check  "
            if (yosysOption.dontShareBySAT) script += s"-noshare  "
          case yosysOption: YosysXilinxOptimizeOption =>
            if (yosysOption.isFlatten) script += s"-flatten  "
          case _ => script += s""
        }
        script += s"\n"

      case xilinx: XilinxDevice =>
        script += s"synth_xilinx  -top ${designInput.getTopModuleName()}  "
        xilinx.deviceFamily match {
          case UltraScale     => script += s"-family xcu  "
          case Series7        => script += s"-family xc7  "
          case UltraScalePlus => script += s"-family xcup  "
          case _              => script += s""
        }
        optimizeOption match {
          case yosysOption: YosysGeneralOptimizeOption =>
            if (yosysOption.isFlatten) script += s"-flatten  "
          case yosysOption: YosysXilinxOptimizeOption =>
            if (yosysOption.isFlatten) script += s"-flatten  "
            if (!yosysOption.useBram) script += s"-nobram  "
            if (!yosysOption.useDram) script += s"-nolutram  "
            if (!yosysOption.useDSRL) script += s"-nosrl  "
            if (!yosysOption.useCarry) script += s"-nocarry  "
            if (!yosysOption.useMUXF) script += s"-nowidelut  "
            if (!yosysOption.useDsp) script += s"-nodsp  "
            if (!yosysOption.useIOPad) script += s"-noiopad  "
            if (!yosysOption.autoClockBuf) script += s"-noclkbuf  "
            if (yosysOption.retiming) script += s"-retime  "
            xilinx.deviceFamily match {
              case UltraScalePlus =>
                yosysOption.useUram match {
                  case Some(bool) => if (bool) script += s"-uram  "
                  case _          =>
                }
              case _ =>
            }
          case _ => script += s""
        }
        script += s"\n"
      case altera: AlteraDevice =>
        script += s"synth_intel\n"
      case _ => script += s"synth\n"
    }

    FileUtils.write(yosysScript, script)
    yosysLogger.info(s"Finish Yosys script generation...")
    script
  }

  override def genScript(): String = {
    genYosysScript()
    yosysLogger.info(s"Generating Shell script for running Yosys...")
    val script = s"yosys ${yosysScript.getAbsolutePath} > ${logFile.getAbsolutePath} 2>&1"
    FileUtils.write(shellFile, script)
    yosysLogger.info(s"Finish Shell script generation...")
    script
  }

  override def runScript(): Unit = {
    yosysLogger.info(s"Running YosysFlow...")
    DoCmd.doCmd(s"bash ${shellFile.getAbsolutePath}")
    yosysLogger.info(s"Finish YosysFlow...")
  }

  def startFlow(): Unit = {
    genScript()
    runScript()
    YosysReport(logFile, device).genInfosReport()
  }

}
