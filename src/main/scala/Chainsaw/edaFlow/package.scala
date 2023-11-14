package Chainsaw

import Chainsaw.edaFlow.vivado.VivadoUtil
import org.apache.commons.io.FileUtils
import spinal.core._

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import scala.io.Source
import scala.util.Try

package object edaFlow {

  sealed trait Vendor
  sealed trait Family

  object Xilinx extends Vendor
  object Altera extends Vendor

  sealed trait XilinxDeviceFamily extends Family
  object UltraScale extends XilinxDeviceFamily
  object UltraScalePlus extends XilinxDeviceFamily
  object Series7 extends XilinxDeviceFamily

  sealed trait AlteraDeviceFamily extends Family

  object CycloneV extends AlteraDeviceFamily

  object Generic extends Family

  class Device(val vendor: Vendor, val deviceFamily: Family, val familyPart: String) {}

  case class XilinxDevice(
      family: XilinxDeviceFamily,
      part: String,
      fMax: HertzNumber,
      xdcFile: Option[File],
      budget: VivadoUtil = VivadoUtil()
  ) extends Device(Xilinx, family, part) {

    def bramCapacity = budget.bram36 * 36 * 1024 / pow2(20).toDouble

    def uramCapacity = budget.uram288 * 288 * 1024 / pow2(20).toDouble

    def onChipStorage = bramCapacity + uramCapacity

  }

  val vu9p = XilinxDevice(
    UltraScale,
    "xcvu9p-flga2104-2-i",
    600 MHz,
    None,
    budget = VivadoUtil(lut = 1182240, ff = 2364480, dsp = 6840, bram36 = 2160, uram288 = 960, carry8 = 147780)
  )
  val zcu104  = XilinxDevice(UltraScale, "xczu7ev-ffvc1156-2-e", 200 MHz, None)
  val u250    = XilinxDevice(UltraScale, "XCU250-FIGD2104-2L-E".toLowerCase, 600 MHz, None)
  val u200    = XilinxDevice(UltraScale, "XCU200-FSGD2104-2-E".toLowerCase, 600 MHz, None, budget = vu9p.budget)
  val kcu1500 = XilinxDevice(UltraScale, "xcku115-flvb2104-2-e", 800 MHz, None)

  val xilinxCDConfig = ClockDomainConfig( // recommended by Xilinx UG901
    clockEdge              = RISING,
    resetKind              = ASYNC,
    resetActiveLevel       = HIGH,
    softResetActiveLevel   = HIGH,
    clockEnableActiveLevel = HIGH
  )

  trait EdaOptimizeOption

  trait Report

  object ParseReportUtils {

    // regEx functions
    private val intFind    = "[0-9]\\d*"
    private val doubleFind = "-?(?:[1-9]\\d*\\.\\d*|0\\.\\d*[1-9]\\d*|0\\.0+|0)"

    def getPatternAfterHeader(
        header: String,
        pattern: String,
        separator: Char = '|',
        report: String
    ) = {
      val regex =
        s"\\$separator?\\s*$header\\s*\\$separator?\\s*($pattern)\\s*\\$separator?"
      Try(regex.r.findFirstMatchIn(report).get.group(1))
    }

    def getIntAfterHeader(header: String, separator: Char = '|', report: String): Int =
      getPatternAfterHeader(header, intFind, separator, report).getOrElse("-1").toInt

    def getDoubleAfterHeader(header: String, separator: Char = ':', report: String): Double =
      getPatternAfterHeader(header, doubleFind, separator, report)
        .getOrElse("-1")
        .toDouble
  }

  object EdaDirectoryUtils {
    def genTargetWorkspace(target: String, topModuleName: Option[String], workspaceDir: File): File = {
      topModuleName match {
        case Some(name) => new File(workspaceDir, s"gen${target}_$name")
        case None =>
          val dateFormat = new SimpleDateFormat("yyyyMMdd")
          val cla        = Calendar.getInstance()
          cla.setTimeInMillis(System.currentTimeMillis())
          val date   = dateFormat.format(cla.getTime)
          val hour   = cla.get(Calendar.HOUR_OF_DAY).toString
          val min    = cla.get(Calendar.MINUTE).toString
          val second = cla.get(Calendar.SECOND).toString
          new File(workspaceDir, s"gen${target}_InferredTop_${date}_${hour}_${min}_$second")
      }
    }

    def genRtlSourcesFromComponent(
        design: => Component,
        customizedConfig: Option[SpinalConfig] = None,
        topModuleName: Option[String],
        workspaceDir: File,
        fileTypeFilter: Seq[String]
    ): Seq[File] = {

      val genRtlDir = genTargetWorkspace("Rtl", topModuleName, workspaceDir)
      if (genRtlDir.exists()) genRtlDir.delete()

      val config = customizedConfig match {
        case Some(value) => value
        case None => // for general Component
          val config = SpinalConfig(
            defaultConfigForClockDomains = xilinxCDConfig,
            targetDirectory              = s"${genRtlDir.getAbsolutePath}/",
            oneFilePerComponent          = true
          )
          config.addTransformationPhase(new phases.FfIo)
      }

      config.generateVerilog(design)
      if (customizedConfig.isDefined) FileUtils.copyDirectory(new File(config.targetDirectory), genRtlDir)
      if (config.oneFilePerComponent) {
        val lstFile = Source.fromFile(
          new File(
            genRtlDir
              .listFiles()
              .toSeq
              .map(_.getAbsolutePath)
              .filter(path => path endsWith ".lst")
              .head
          )
        )

        lstFile
          .getLines()
          .map { line => new File(line) }
          .map(_.getAbsolutePath)
          .toSeq
          .map(new File(_))

      } else {
        val designDir = new File(config.targetDirectory)
        designDir
          .listFiles()
          .toSeq
          .map(_.getAbsolutePath)
          .filter(path => fileTypeFilter.exists(fileType => path endsWith fileType))
          .map(new File(_))
      }
    }
  }

  abstract class EdaFlow(
      designDirs: Seq[File],
      workspaceDir: File,
      topModuleName: Option[String],
      deviceType: Option[Device],
      taskType: EdaFlowType,
      optimizeOption: EdaOptimizeOption,
      blackBoxSet: Option[Set[String]],
      memBinaryFile: Option[Map[String, File]]
  ) {

    def genScript(): String

    def runScript(): Unit

  }

}
