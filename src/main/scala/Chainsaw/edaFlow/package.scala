package Chainsaw

import Chainsaw.edaFlow.EdaFlowUtils.EdaDirectoryUtils._
import Chainsaw.edaFlow.vivado.VivadoUtil
import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.core.internals.PhaseContext

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Try

package object edaFlow {

  sealed trait Vendor
  object Xilinx extends Vendor
  object Altera extends Vendor

  sealed trait Family

  sealed trait XilinxDeviceFamily extends Family
  object UltraScale extends XilinxDeviceFamily
  object UltraScalePlus extends XilinxDeviceFamily
  object Series7 extends XilinxDeviceFamily

  sealed trait AlteraDeviceFamily extends Family

  object CycloneV extends AlteraDeviceFamily

  sealed trait GenericFamily extends Family

  object Generic extends GenericFamily

  case class ChainsawDevice(
      vendor: Vendor,
      deviceFamily: Family,
      familyPart: String,
      fMax: HertzNumber,
      xdcFile: Option[File],
      budget: VivadoUtil = VivadoUtil()
  ) {}

  class XilinxDevice(
      family: XilinxDeviceFamily,
      part: String,
      fMax: HertzNumber,
      xdcFile: Option[File],
      budget: VivadoUtil = VivadoUtil()
  ) extends ChainsawDevice(Xilinx, family, part, fMax, xdcFile, budget) {

    def bramCapacity = budget.bram36 * 36 * 1024 / pow2(20).toDouble

    def uramCapacity = budget.uram288 * 288 * 1024 / pow2(20).toDouble

    def onChipStorage = bramCapacity + uramCapacity

  }

  class AlteraDevice(
      family: AlteraDeviceFamily,
      part: String,
      fMax: HertzNumber,
      xdcFile: Option[File],
      budget: VivadoUtil = VivadoUtil()
  ) extends ChainsawDevice(Altera, family, part, fMax, xdcFile, budget) {

    def bramCapacity = budget.bram36 * 36 * 1024 / pow2(20).toDouble

    def uramCapacity = budget.uram288 * 288 * 1024 / pow2(20).toDouble

    def onChipStorage = bramCapacity + uramCapacity

  }

  class GenericDevice(
      family: GenericFamily,
      part: String,
      fMax: HertzNumber,
      xdcFile: Option[File],
      budget: VivadoUtil = VivadoUtil()
  ) extends ChainsawDevice(Xilinx, family, part, fMax, xdcFile, budget) {

    def bramCapacity = budget.bram36 * 36 * 1024 / pow2(20).toDouble

    def uramCapacity = budget.uram288 * 288 * 1024 / pow2(20).toDouble

    def onChipStorage = bramCapacity + uramCapacity

  }

  val vu9p = new XilinxDevice(
    UltraScale,
    "xcvu9p-flga2104-2-i",
    300 MHz,
    None,
    budget = VivadoUtil(lut = 1182240, ff = 2364480, dsp = 6840, bram36 = 2160, uram288 = 960, carry8 = 147780)
  )
  val zcu104  = new XilinxDevice(UltraScale, "xczu7ev-ffvc1156-2-e", 200 MHz, None)
  val u250    = new XilinxDevice(UltraScale, "XCU250-FIGD2104-2L-E".toLowerCase, 600 MHz, None)
  val u200    = new XilinxDevice(UltraScale, "XCU200-FSGD2104-2-E".toLowerCase, 600 MHz, None, budget = vu9p.budget)
  val kcu1500 = new XilinxDevice(UltraScale, "xcku115-flvb2104-2-e", 800 MHz, None)
  val a7100t = new XilinxDevice(
    Series7,
    "XC7A100T-CSG324-1".toLowerCase(),
    100 MHz,
    Some(new File(xdcFileDir, "NexysA7100T/NexysA7100T-activatedPins.xdc")),
    budget = VivadoUtil(lut = 32600, ff = 65200, dsp = 240)
  )
  val generic = new GenericDevice(Generic, "", 600 MHz, None)

  val xilinxDefaultCDConfig = ClockDomainConfig( // recommended by Xilinx UG901
    clockEdge              = RISING,
    resetKind              = ASYNC,
    resetActiveLevel       = HIGH,
    softResetActiveLevel   = HIGH,
    clockEnableActiveLevel = HIGH
  )

  val xilinxDefaultSpinalConfig = SpinalConfig(mode = Verilog, defaultConfigForClockDomains = xilinxDefaultCDConfig)

  abstract class ChainsawEdaFlowInput[T <: Module](
      design: => T,
      val designDirs: Seq[File],
      val workspaceDir: File,
      val topModuleName: String,
      val customizedConfig: Option[SpinalConfig] = None
  ) {
    def module = design

    def getRtlDir(supportedFileTypes: Seq[String] = Seq(".v", ".sv")): Seq[File]
  }

  class ChainsawEdaModuleInput[T <: Module](
      design: => T,
      workspaceDir: File,
      topModuleName: String,
      customizedConfig: Option[SpinalConfig] = None
  ) extends ChainsawEdaFlowInput(design, Seq[File](), workspaceDir, topModuleName, customizedConfig) {

    var finishRtlDirGen                 = false
    val rtlResources: ArrayBuffer[File] = ArrayBuffer()

    def getRtlDir(supportedFileTypes: Seq[String] = Seq(".v", ".sv")): Seq[File] = {
      if (!finishRtlDirGen) {
        rtlResources ++= genRtlSourcesFromComponent(
          design,
          customizedConfig,
          topModuleName,
          workspaceDir,
          supportedFileTypes
        )
        finishRtlDirGen = true
      }
      rtlResources
    }
  }

  object ChainsawEdaModuleInput {
    def apply[T <: Module](
        design: => T,
        workspaceDir: File,
        topModuleName: String,
        customizedConfig: Option[SpinalConfig] = None
    ) = new ChainsawEdaModuleInput(design, workspaceDir, topModuleName, customizedConfig)
  }

  class ChainsawEdaDirInput(
      designDirs: Seq[File],
      workspaceDir: File,
      topModuleName: String
  ) extends ChainsawEdaFlowInput(null, Seq[File](), workspaceDir, topModuleName, None) {

    var finishRtlDirGen                 = false
    val rtlResources: ArrayBuffer[File] = ArrayBuffer()

    def getRtlDir(supportedFileTypes: Seq[String] = Seq(".v", ".sv")): Seq[File] = {
      if (!finishRtlDirGen) {
        rtlResources ++= parseRtlDirFor(designDirs, supportedFileTypes)
        finishRtlDirGen = true
      }
      rtlResources
    }
  }

  object ChainsawEdaDirInput {
    def apply[T <: Module](
        designDirs: Seq[File],
        workspaceDir: File,
        topModuleName: String
    ) = new ChainsawEdaDirInput(designDirs, workspaceDir, topModuleName)
  }

  class ChainsawEdaFullInput[T <: Module](
      design: => T,
      designDirs: Seq[File],
      workspaceDir: File,
      topModuleName: String,
      customizedConfig: Option[SpinalConfig] = None
  ) extends ChainsawEdaFlowInput(design, designDirs, workspaceDir, topModuleName, customizedConfig) {

    var finishRtlDirGen                 = false
    val rtlResources: ArrayBuffer[File] = ArrayBuffer()

    def getRtlDir(supportedFileTypes: Seq[String] = Seq(".v", ".sv")): Seq[File] = {
      if (!finishRtlDirGen) {
        rtlResources ++= parseRtlDirFor(designDirs, supportedFileTypes)
        rtlResources ++= genRtlSourcesFromComponent(
          design,
          customizedConfig,
          topModuleName,
          workspaceDir,
          supportedFileTypes
        )
        finishRtlDirGen = true
      }
      rtlResources
    }
  }

  object ChainsawEdaFullInput {
    def apply[T <: Module](
        design: => T,
        designDirs: Seq[File],
        workspaceDir: File,
        topModuleName: String,
        customizedConfig: Option[SpinalConfig] = None
    ) = new ChainsawEdaFullInput(design, designDirs, workspaceDir, topModuleName, customizedConfig)
  }

  trait EdaOptimizeOption

  trait Report

  abstract class EdaFlow(
      designDirs: Seq[File],
      workspaceDir: File,
      topModuleName: String,
      device: ChainsawDevice,
      taskType: EdaFlowType,
      optimizeOption: EdaOptimizeOption,
      blackBoxSet: Option[Set[String]],
      memBinaryFile: Option[Map[String, File]]
  ) {

    def genScript(): String

    def runScript(): Unit

  }

  def inVirtualGlob[T](func: => T): T = {
    val old = GlobalData.get

    val virtualGlob = new GlobalData(SpinalConfig())
    virtualGlob.phaseContext = new PhaseContext(SpinalConfig())
    GlobalData.set(virtualGlob)
    val ret = func

    GlobalData.set(old)
    ret
  }

}
