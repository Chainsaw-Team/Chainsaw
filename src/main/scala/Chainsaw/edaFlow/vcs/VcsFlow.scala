package Chainsaw.edaFlow.vcs

import Chainsaw._
import Chainsaw.edaFlow.Device.generic
import Chainsaw.edaFlow._

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import org.apache.commons.io.FileUtils
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._
import org.scalatest.funsuite._
import org.slf4j._
import spinal.lib.DoCmd
import spinal.sim.VCSFlags

/** The class which can be used to run VcsTask
  * @param designInput
  *   the vcsFlow design source file input
  * @param compileOption
  *   specify the optimize option will be used in vcsFlow
  * @param customizedConfig
  *   specify SpinalConfig will used in VcsFlow
  * @param macroFile
  *   the macro file will be add to VcsFlow
  */
case class VcsFlow(
    designInput: ChainsawEdaDirInput,
    compileOption: VcsCompileOption,
    customizedConfig: Option[SpinalConfig] = None,
    macroFile: Option[Seq[File]]           = None
) extends EdaFlow(
      designDirs     = designInput.designDirs,
      workspaceDir   = designInput.workspaceDir,
      topModuleName  = designInput.topModuleName,
      device         = generic,
      taskType       = SIM,
      optimizeOption = compileOption,
      blackBoxSet    = None
    ) {

  require(
    VCS.exist(),
    "to use VcsFlow, please set the environment variable 'VCS' to the vcs executable, e.g. /opt/Synopsys/vcs201809/bin"
  )

  val vcsLogger = LoggerFactory.getLogger(s"VcsFlow")

  val vcsWorkDir    = new File(designInput.workspaceDir, s"genByVcsFlow_${designInput.topModuleName}")
  var compileFlag   = ArrayBuffer[String]()
  var elaborateFlag = ArrayBuffer[String]()
  var runSimFlag    = ArrayBuffer[String]()

  val config = customizedConfig match {
    case Some(value) => value
    case None => // for general Component
      val config = SpinalConfig(
        defaultConfigForClockDomains = xilinxDefaultCDConfig,
        targetDirectory              = s"${vcsWorkDir.getAbsolutePath}/rtl/",
        oneFilePerComponent          = true
      )
      config.addTransformationPhase(new phases.FfIo)
  }

  override def genScript(): String = {

    val fileList               = new File(vcsWorkDir, "includeFileList.f")
    val coverageHierConfigFile = new File(vcsWorkDir, "cov_config.cfg")

    // generate fileList by includeDirs(if exist)
    val flattenDirs        = ArrayBuffer[String]()
    val supportedFileTypes = Seq(".v", ".sv")
    designInput.designDirs.foreach { dir =>
      if (dir.getAbsolutePath.endsWith(".f") || dir.getAbsolutePath.endsWith(".lst")) {
        val listFile = Source.fromFile(dir)
        listFile
          .getLines()
          .map { line => new File(line) }
          .map(_.getAbsolutePath)
          .toSeq
          .foreach(path => flattenDirs.append(path))
      } else {
        if (supportedFileTypes.exists(filrType => dir.getAbsolutePath.endsWith(filrType)))
          flattenDirs.append(dir.getAbsolutePath)
      }
    }
    FileUtils.write(fileList, flattenDirs.mkString("\n"))

    // for coverage
    FileUtils.write(
      coverageHierConfigFile,
      flattenDirs
        .filter(dir => dir.endsWith(".v") || dir.endsWith(".sv"))
        .map(matchDir => s"-file $matchDir")
        .mkString("\n")
    )

    compileFlag.append(s"-j${compileOption.parallelNumber}")
    compileFlag.append("-V")
    compileFlag.append("-notice")
    compileFlag.append(s"-kdb")
    if (designInput.designDirs.nonEmpty) compileFlag.append(s"-f ${fileList.getAbsolutePath}")
    if (macroFile.isDefined) compileFlag.append(s"-f ${macroFile.get.map(_.getAbsoluteFile).mkString(" ")}")
    compileFlag.append(s"-top ${designInput.topModuleName}")

    elaborateFlag.append("-LDFLAGS -Wl,--no-as-needed")
    elaborateFlag.append(s"-j${compileOption.parallelNumber}")
    elaborateFlag.append("-kdb")
    elaborateFlag.append("-notice")
    elaborateFlag.append("-lca")
    compileOption.enableCoverageType.foreach {
      case LineCoverage =>
        elaborateFlag.append(s"-cm line")
        runSimFlag.append(s"-cm line")
      case CondCoverage =>
        elaborateFlag.append(s"-cm cond")
        runSimFlag.append(s"-cm cond")
      case FsmCoverage =>
        elaborateFlag.append(s"-cm fsm")
        runSimFlag.append(s"-cm fsm")
      case TglCoverage =>
        elaborateFlag.append(s"-cm tgl")
        runSimFlag.append(s"-cm tgl")
      case PathCoverage =>
        elaborateFlag.append(s"-cm path")
        runSimFlag.append(s"-cm path")
      case BranchCoverage =>
        elaborateFlag.append(s"-cm branch")
        runSimFlag.append(s"-cm branch")
      case AssertCoverage =>
        elaborateFlag.append(s"-cm assert")
        runSimFlag.append(s"-cm assert")
      case FullCoverage =>
        elaborateFlag.append(s"-cm line+cond+fsm+tgl+path+branch+assert")
        runSimFlag.append(s"-cm line+cond+fsm+tgl+path+branch+assert")
    }
    if (designInput.designDirs.nonEmpty)
      elaborateFlag.append(s"-cm_assert_hier ${coverageHierConfigFile.getAbsolutePath}")
    elaborateFlag.append(s"-cm_dir ${vcsWorkDir.getAbsolutePath}/cov.vdb")
    elaborateFlag.append(s"-cm_name cov_${vcsWorkDir.getName}")
    elaborateFlag.append(s"-cm_log cm_compile.log")
    elaborateFlag.append(s"-top ${designInput.topModuleName}")
    if (compileOption.incrementCompile) elaborateFlag.append("-M")
    if (compileOption.enableMemHierarchy) elaborateFlag.append("+memcbk")
    if (compileOption.noTimingCheck) elaborateFlag.append("+notimingcheck")

    runSimFlag.append(s"-cm_dir ${vcsWorkDir.getAbsolutePath}/cov.vdb")
    runSimFlag.append(s"-cm_name cov_${vcsWorkDir.getName}")
    runSimFlag.append(s"-cm_log cm_sim.log")
    runSimFlag.append("-V")
    if (compileOption.noTimingCheck) runSimFlag.append("+notimingcheck")
    runSimFlag.append(s"-l simv.log")

    compileFlag.mkString(" ") + "\n" + elaborateFlag.mkString(" ") + "\n" + runSimFlag.mkString(" ")
  }

  override def runScript(): Unit = {
    val scriptContent = genScript().split("\n")
    vcsLogger.info(
      s"generate script for SpinalHDL vcs api:\ncompileFlag: ${scriptContent.head}\nelaborateFlags: ${scriptContent.tail.head}\nrunSimFlag: ${scriptContent.last}\n"
    )
  }

  def getSpinalSimConfig(withWave: Boolean = true): SpinalSimConfig = {
    runScript()
    val ret = SimConfig
      .withVCS(VCSFlags(compileFlag.toList, elaborateFlag.toList, runSimFlag.toList))
      .workspacePath(vcsWorkDir.getAbsolutePath)
    if (withWave) ret.withFSDBWave else ret
  }

}
