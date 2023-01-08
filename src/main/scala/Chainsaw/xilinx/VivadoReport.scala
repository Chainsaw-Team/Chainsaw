package Chainsaw.xilinx

import spinal.core._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try
import Chainsaw._
import Chainsaw.xilinx.VivadoReport.getUltrascaleWithHierarchy

import java.io.File

// TODO: for more robust extraction, .rpt files, rather than .log files should be used

/** this class is designed to extract information from Vivado log file(synth or
  * impl)
  */
class VivadoReport(
    logFile: File,
    deviceFamily: XilinxDeviceFamily
) {

  val log            = Source.fromFile(logFile)
  val lines          = log.getLines.toSeq
  private val report = lines.mkString("\n")

  // regEx functions
  private val intFind    = "[0-9]\\d*"
  private val doubleFind = "-?(?:[1-9]\\d*\\.\\d*|0\\.\\d*[1-9]\\d*|0\\.0+|0)"

  def getPatternAfterHeader(
      header: String,
      pattern: String,
      separator: Char = '|'
  ) = {
    val regex =
      s"\\$separator?\\s*$header\\s*\\$separator?\\s*($pattern)\\s*\\$separator?"
    Try(regex.r.findFirstMatchIn(report).get.group(1))
  }

  def getIntAfterHeader(header: String, separator: Char = '|'): Int =
    getPatternAfterHeader(header, intFind, separator).getOrElse("-1").toInt

  def getDoubleAfterHeader(header: String, separator: Char = ':'): Double =
    getPatternAfterHeader(header, doubleFind, separator)
      .getOrElse("-1")
      .toDouble

  // get fMax
  val ConstraintPeriod = getDoubleAfterHeader("Requirement")
  val Slack            = getDoubleAfterHeader(s"Slack\\s*\\(\\w*\\)")
  val Frequency        = 1.0 / (ConstraintPeriod - Slack) * 1e9

  // get util
  val util = deviceFamily match {
    case xilinx.UltraScale => getUltrascaleWithHierarchy(logFile)
    case xilinx.Series7    => ???
  }

  override def toString: String =
    s"$util, Freq = ${Frequency / 1e6} MHz\n"

  /** -------- requirements
    * --------
    */
  def requireFmax(fmaxRequirement: HertzNumber): Unit = {
    assert(
      this.Frequency >= fmaxRequirement.toDouble,
      s"\ncritical path failed: \n\tyours:  ${Frequency / 1e6} MHz, \n\ttarget: ${fmaxRequirement.toDouble / 1e6} MHz"
    )
    logger.info(
      s"\ncritical path met: \n\tyours:  ${Frequency / 1e6} MHz, \n\ttarget: ${fmaxRequirement.toDouble / 1e6} MHz"
    )
  }

  def requireUtil(
      utilRequirement: VivadoUtil,
      utilRequirementStrategy: UtilRequirementStrategy
  ): Unit = {
    val pass = utilRequirementStrategy match {
      case DefaultRequirement =>
        val relaxedRequirement = (utilRequirement * 1.05) // 5% tolerant
          .withFf(Double.MaxValue) // don't care ff
          .withCarry(Double.MaxValue)
        if (this.util.ff > 2 * this.util.lut)
          logger.warn(s"ff > 2 * lut, this may lead to extra clb consumption")
        this.util <= relaxedRequirement
      case PreciseRequirement => this.util <= utilRequirement
      case NoRequirement      => true
    }

    assert(
      pass,
      s"\nutil failed: \n\tyours:  $util, \n\ttarget: $utilRequirement"
    )
    logger.info(s"\nutil met: \n\tyours:  $util, \n\ttarget: $utilRequirement")
  }

  log.close()
}

object VivadoReport extends App {

  def getUltrascaleWithHierarchy(logFile: File): VivadoUtil = {
    val log   = Source.fromFile(logFile)
    val lines = log.getLines.toSeq

    val start = lines.lastIndexWhere(_.contains("Utilization by Hierarchy"))
    var tab   = 0
    val topLine = lines
      .drop(start)
      .dropWhile { line =>
        if (line.startsWith("+")) tab += 1
        tab < 2
      }(1) // TODO: extract info of sub-modules

    val segments = topLine.split("\\|").map(_.trim)
    val lut      = segments(4).toInt // logic LUT only
    val ff       = segments(7).toInt
    val bram36   = segments(8).toInt
    val uram288  = segments(10).toInt
    val dsp      = segments(11).toInt

    // TODO: carry extraction
    VivadoUtil(
      lut     = lut,
      ff      = ff,
      dsp     = dsp,
      bram36  = bram36,
      uram288 = uram288,
      carry8  = 0
    )
  }
}
