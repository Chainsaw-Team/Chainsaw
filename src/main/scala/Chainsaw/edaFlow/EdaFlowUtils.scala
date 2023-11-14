package Chainsaw.edaFlow

import Chainsaw.phases
import org.apache.commons.io.FileUtils
import spinal.core.{Component, SpinalConfig}

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
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
//    def genTargetWorkspace(target: String, topModuleName: Option[String], workspaceDir: File): File = {
//      topModuleName match {
//        case Some(name) => new File(workspaceDir, s"gen${target}_$name")
//        case None =>
//          val dateFormat = new SimpleDateFormat("yyyyMMdd")
//          val cla        = Calendar.getInstance()
//          cla.setTimeInMillis(System.currentTimeMillis())
//          val date   = dateFormat.format(cla.getTime)
//          val hour   = cla.get(Calendar.HOUR_OF_DAY).toString
//          val min    = cla.get(Calendar.MINUTE).toString
//          val second = cla.get(Calendar.SECOND).toString
//          new File(workspaceDir, s"gen${target}_InferredTop_${date}_${hour}_${min}_$second")
//      }
//    }

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

    def genRtlSourcesFromComponent(
        design: => Component,
        customizedConfig: Option[SpinalConfig] = None,
        topModuleName: String,
        workspaceDir: File,
        fileTypeFilter: Seq[String]
    ): Seq[File] = {

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

      config.generateVerilog(design)
      if (customizedConfig.isDefined) {
        if (genRtlDir.exists()) genRtlDir.delete()
        FileUtils.copyFileToDirectory(new File(config.targetDirectory, s"$topModuleName.v"), genRtlDir)
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

        lstFile
          .getLines()
          .map { line => new File(line) }
          .map(_.getAbsolutePath)
          .toSeq
          .map(new File(_))

      } else {
        genRtlDir
          .listFiles()
          .toSeq
          .map(_.getAbsolutePath)
          .filter(path => fileTypeFilter.exists(fileType => path endsWith fileType))
          .map(new File(_))
      }
    }
  }
}
