package Chainsaw.edaFlow

import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow.EdaFlowUtils.ParseReportUtils
import org.slf4j._

import java.io.File
import scala.io.Source

package object yosys {

  sealed trait YosysOptimizeOption extends EdaOptimizeOption

  /** the OptimizeOption in YosysFlow when not specify device
    * @param isFlatten
    *   chose whether flatten design
    * @param fsmOptimization
    *   chose whether do fsm optimization
    * @param lutArchitecture
    *   chose the LUT Architecture will be used, for example, in Xilinx FPGA, it should be 6
    * @param keepOpDirectForm
    *   chose whether keep all operator naive form(not optimization)
    * @param absorbMemDff
    *   chose whether absorb the FF which driven by memory in memory
    * @param dontCareRWCollision
    *   chose whether can ignore Read-Write collision
    * @param dontShareBySAT
    *   chose whether can share logic by SAT(Boolean satisfiability problem)
    */
  case class YosysGeneralOptimizeOption(
      isFlatten: Boolean           = true,
      fsmOptimization: Boolean     = true,
      lutArchitecture: Int         = 6,
      keepOpDirectForm: Boolean    = false,
      absorbMemDff: Boolean        = true,
      dontCareRWCollision: Boolean = true,
      dontShareBySAT: Boolean      = false
  ) extends YosysOptimizeOption

  /** the Xilinx device OptimizeOption in YosysFlow
    * @param isFlatten
    *   chose whether flatten design in output netlist
    * @param useBram
    *   chose whether use Block Ram in output netlist
    * @param useDram
    *   chose whether use Distribute Ram in output netlist
    * @param useDSRL
    *   chose whether use distributed SRL cells in output netlist
    * @param useCarry
    *   chose whether use XORCY/MUXCY/CARRY4 cells in output netlist
    * @param useMUXF
    *   chose whether use MUXFresources to implement LUTs larger than native for the target
    * @param useDsp
    *   chose whether use Dsp in output netlist
    * @param useIOPad
    *   chose whether enable I/O buffer insertion (useful for hierarchical or out-of-context flows)
    * @param autoClockBuf
    *   chose whether use automatic clock buffer insertion in output netlist
    * @param retiming
    *   chose whether enable flip-flop retiming in output netlist
    * @param useUram
    *   chose whether use Ultra Ram in output netlist (xcup only)
    */
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
