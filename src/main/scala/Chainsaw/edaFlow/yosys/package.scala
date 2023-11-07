package Chainsaw.edaFlow

import Chainsaw.edaFlow.EdaFlowUtils.ParseReportUtils
import org.slf4j._

import java.io.File
import scala.io.Source

package object yosys {

  sealed trait YosysOptimizeOption extends EdaOptimizeOption

  case class YosysGeneralOptimizeOption(
      isFlatten: Boolean           = true,
      fsmOptimization: Boolean     = true,
      lutArchitecture: Int         = 6,
      keepOpDirectForm: Boolean    = false,
      absorbMemDff: Boolean        = true,
      dontCareRWCollision: Boolean = true,
      dontShareBySAT: Boolean      = false
  ) extends YosysOptimizeOption

  case class YosysXilinxOptimizeOption(
      isFlatten: Boolean       = true,
      useBram: Boolean         = true,
      useDram: Boolean         = true,
      useDSRL: Boolean         = true,
      useCarry: Boolean        = true,
      useMUXF: Boolean         = true,
      useDsp: Boolean          = true,
      useIOPad: Boolean        = true,
      autoClockBuf: Boolean    = true,
      retiming: Boolean        = true,
      useUram: Option[Boolean] = None
  ) extends YosysOptimizeOption

  case class YosysReport(logFile: File, targetDevice: ChainsawDevice) extends Report {
    def genInfosReport(): Unit = {
      val parseLogger = LoggerFactory.getLogger(s"Get utilization information in log file which generating by Yosys")

      val log    = Source.fromFile(logFile)
      val lines  = log.getLines.toSeq
      val report = lines.mkString("\n")

      parseLogger.info(s"Parsing utilization information in ${logFile.getName}")
      val parseItem = targetDevice match {
        case generic: GenericDevice =>
          Seq(
            "_ANDNOT_",
            "_DFFE_PP0P_ ",
            "_DFFE_PP_",
            "_DFF_PP0_",
            "_DFF_P_",
            "_MUX_",
            "_NAND_",
            "_NOR_",
            "_NOT_",
            "_ORNOT_",
            "_OR_",
            "_XNOR_",
            "_XOR_"
          )
        case xilinx: XilinxDevice =>
          Seq(
            "BUFG",
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
            "IBUF",
            "OBUF",
            "RAM32M"
          )
        case altera: AlteraDevice =>
          Seq(
            "BUFG",
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
            "IBUF",
            "OBUF",
            "RAM32M"
          )
        case _ =>
          Seq()

      }
      val parseValues = parseItem.map { item =>
        ParseReportUtils.getIntAfterHeader(header = item, separator = ':', report = report)
      }

      // making table
      val itemWidth        = parseItem.map(_.length).max max 6
      val valuesWidth: Int = parseValues.map(_.toString.length).max max 8
      val header =
        s"|" + s"${" " * ((itemWidth - 4) / 2)}cell${" " * ((itemWidth - 3) / 2)}" +
          s"|" + s"${" " * ((valuesWidth - 6) / 2)}number${" " * ((valuesWidth - 5) / 2)}" + s"|\n" +
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
