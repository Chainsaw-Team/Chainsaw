package Chainsaw.edaFlow

import org.slf4j.LoggerFactory

import java.io.File
import scala.io.Source

package object vivado {

  case class VivadoOptimizeOption(
      isFlatten: Boolean           = true,
      convertClkGateLogic: Boolean = true,
      enableLutComb: Boolean       = true,
      fsmRecode: Boolean           = true,
      keepEquReg: Boolean          = true,
      resourceShare: Boolean       = true,
      reTiming: Boolean            = true,
      maxFanout: Int               = -1,
      maxclkBuf: Int               = -1,
      maxBram: Int                 = -1,
      maxUram: Int                 = -1,
      maxDsp: Int                  = -1
  ) extends EdaOptimizeOption

  case class VivadoReport(logFile: File, deviceType: Device) extends Report {

    def genInfosReport(): Unit = {
      val parseLogger =
        LoggerFactory.getLogger(s"Get utilization and timing information in log file which generating by Vivado")

      val log    = Source.fromFile(logFile)
      val lines  = log.getLines.toSeq
      val report = lines.mkString("\n")

      parseLogger.info(s"Parsing utilization information in ${logFile.getName}")

      val ConstraintPeriod = ParseReportUtils.getDoubleAfterHeader(header = "Requirement", report = report)
      val Slack            = ParseReportUtils.getDoubleAfterHeader(header = s"Slack\\s*\\(\\w*\\)", report = report)
      val Frequency        = 1.0 / (ConstraintPeriod - Slack) * 1e9

      val parseItem = Seq(
        "Frequency",
        "CARRY4",
        "CARRY8",
        "FDCE",
        "FDRE",
        "LUT2",
        "LUT3",
        "LUT4",
        "LUT5",
        "LUT6",
        "MUXF7",
        "RAM32M16"
      )

      val parseValues =
        Seq(Frequency) ++ parseItem.tail.map(item => ParseReportUtils.getIntAfterHeader(header = item, report = report))

      // making table
      val itemWidth        = parseItem.map(_.length).max max 6
      val valuesWidth: Int = parseValues.map(_.toString.length).max max 9
      val header =
        s"|" + s"${" " * ((itemWidth - 4) / 2)}item${" " * ((itemWidth - 3) / 2)}" +
          s"|" + s"${" " * ((valuesWidth - 5) / 2)}value${" " * ((valuesWidth - 4) / 2)}" + s"|\n" +
          s"|" + s"${" " * ((itemWidth - 4) / 2)}----${" " * ((itemWidth - 3) / 2)}" +
          s"|" + s"${" " * ((valuesWidth - 6) / 2)}------${" " * ((valuesWidth - 5) / 2)}" + s"|\n"
      val body = parseItem
        .zip(parseValues)
        .map { case (cell, number) =>
          s"|" + s"${" " * ((itemWidth - cell.length) / 2)}$cell${" " * ((itemWidth - cell.length + 1) / 2)}" +
            s"|" + s"${" " * ((valuesWidth - number.toString.length) / 2)}$number${" " * ((valuesWidth - number.toString.length + 1) / 2)}" + s"|"
        }
        .mkString("\n")

      val errorInfo = {
        ParseReportUtils
          .getPatternAfterHeader(header = "ERROR", pattern = ".*", separator = ':', report = report)
          .getOrElse(s"")
      }

      if (errorInfo.isEmpty)
        parseLogger.info(s"Parsing result:\n$header$body")
      else
        parseLogger.error(s"Find Error:\n$errorInfo")
    }
  }
}
