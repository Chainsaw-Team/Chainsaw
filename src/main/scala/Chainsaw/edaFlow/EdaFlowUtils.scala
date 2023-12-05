package Chainsaw.edaFlow

import Chainsaw.edaFlow.Device.ChainsawDevice
import Chainsaw.phases
import org.apache.commons.io.FileUtils
import spinal.core._

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, LinkedHashSet}
import scala.io.Source
import scala.util.Try

object EdaFlowUtils {
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

    def parseRtlDirFor(designDirs: Seq[File], supportedFileTypes: Seq[String] = Seq(".v", ".sv")): Seq[File] = {
      val supportedFiles = designDirs.flatMap { dir =>
        dir
          .listFiles()
          .toSeq
          .map(_.getAbsolutePath)
          .filter(path => supportedFileTypes.exists(fileType => path endsWith fileType))
          .map(new File(_))
      }

      val fileLists = designDirs.flatMap { dir =>
        dir
          .listFiles()
          .toSeq
          .map(_.getAbsolutePath)
          .filter(path => path endsWith ".lst")
          .map(new File(_))
      }

      val flattenFileLists = fileLists.flatMap { list =>
        val listSource = Source.fromFile(list)
        listSource
          .getLines()
          .map { line => new File(line) }
          .map(_.getAbsolutePath)
          .toSeq
          .map(new File(_))
      }

      supportedFiles ++ flattenFileLists

    }

    def genRtlSourcesAndDeviceFrom(
        design: => Component,
        customizedConfig: Option[SpinalConfig] = None,
        topModuleName: String,
        workspaceDir: File,
        fileTypeFilter: Seq[String]
    ): (ArrayBuffer[File], ArrayBuffer[ChainsawDevice], ArrayBuffer[File]) = {

      val resultDirs    = ArrayBuffer[File]()
      val resultDevice  = ArrayBuffer[ChainsawDevice]()
      val resultXdcFile = ArrayBuffer[File]()

      val genRtlDir = new File(workspaceDir, s"genRtl_$topModuleName")
      if (genRtlDir.exists()) genRtlDir.delete()

      val config = customizedConfig match {
        case Some(value) => value
        case None => // for general Component
          val config = SpinalConfig(
            defaultConfigForClockDomains = xilinxDefaultCDConfig,
            targetDirectory              = s"${genRtlDir.getAbsolutePath}/",
            oneFilePerComponent          = true
          )
          config.addTransformationPhase(new phases.FfIo)
      }

      config.generateVerilog {
        inVirtualGlob {
          def parseBlackBoxBlock() = {
            design.walkComponents {
              case blackBox: BlackBox =>
                resultDirs ++= blackBox.listRTLPath.toSeq.map(new File(_))
              case _ =>
            }
          }

          def parseBlock() = design match {
            case board: Component with Board =>
              resultDevice += board.device
              resultXdcFile += board.xdcFile
              parseBlackBoxBlock()
            case _ =>
              parseBlackBoxBlock()
          }

          parseBlock()
        }
        design
      }

      if (customizedConfig.isDefined) {
        if (genRtlDir.exists()) genRtlDir.delete()
        val targetFile = new File(config.targetDirectory)
        FileUtils.copyFileToDirectory(new File(targetFile, s"$topModuleName.v"), genRtlDir)
        val generatedBinFile = targetFile.listFiles().toSeq.filter(_.getAbsolutePath.endsWith(".bin"))
        generatedBinFile.foreach(FileUtils.copyFileToDirectory(_, genRtlDir))
      }

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

        resultDirs ++= lstFile
          .getLines()
          .map { line => new File(line) }
          .map(_.getAbsolutePath)
          .toSeq
          .map(new File(_))

      } else {
        resultDirs ++= genRtlDir
          .listFiles()
          .toSeq
          .map(_.getAbsolutePath)
          .filter(path => fileTypeFilter.exists(fileType => path endsWith fileType))
          .map(new File(_))
      }
      (resultDirs.distinct, resultDevice, resultXdcFile)
    }
  }
}
