package Chainsaw.xilinx

import spinal.core._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try
import Chainsaw._
import java.io.File

/** this class is designed to extract information from Vivado log file(synth or impl)
 */
class VivadoReport(
                    logFile: File,
                    deviceFamily: XilinxDeviceFamily
                  ) {

  // TODO: report number of URAM

  val log = Source.fromFile(logFile)
  val lines = log.getLines.toSeq
  private val report = lines.mkString("\n")

  val patternAdder =
    """\s*(\d+)\s*Input\s*(\d+)\s*Bit\s*Adders :=\s*(\d+)\s*""".r
  val patternReg = """\s*(\d+)\s*Bit\s*Registers\s*:=\s*(\d+)\s*""".r

  // retailed components analysis
  var binaryAdderCost = 0
  var ternaryAdderCost = 0
  var registerCost = 0

  lines.foreach {
    case patternAdder(input, width, number) =>
      if (input.toInt == 2) binaryAdderCost += width.toInt * number.toInt
      if (input.toInt == 3) ternaryAdderCost += width.toInt * number.toInt
    case patternReg(width, number) =>
      registerCost += width.toInt * number.toInt
    case _ =>
  }

  // FIXME: reg cost is obviously wrong

  //  logger.info(s"binary adder cost = $binaryAdderCost")
  //  logger.info(s"ternary adder cost = $ternaryAdderCost")
  //  logger.info(s"reg cost = $registerCost")

  // patterns
  private val intFind = "[0-9]\\d*"
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

  // TODO: extract information from detailed components
  // TODO: adaption for Series7 devices
  val LUT =
    if (deviceFamily == UltraScale) getIntAfterHeader("CLB LUTs\\*?")
    else getIntAfterHeader("Slice LUTs")

  val FF =
    if (deviceFamily == UltraScale) getIntAfterHeader("CLB Registers")
    else getIntAfterHeader("Slice Registers")

  val DSP = getIntAfterHeader("DSPs")
  val BRAM = getIntAfterHeader("Block RAM Tile")
  // FIXME: extract uram correctly
  val URAM = getIntAfterHeader("URAM288")
  val CARRY8 = getIntAfterHeader("CARRY8")

  val ConstraintPeriod = getDoubleAfterHeader("Requirement")
  val Slack = getDoubleAfterHeader(s"Slack\\s*\\(\\w*\\)")

  val Frequency = 1.0 / (ConstraintPeriod - Slack) * 1e9

  val util = VivadoUtil(LUT, FF, DSP, BRAM, URAM, CARRY8)

  def printArea(): Unit = logger.info(
    s"\nLUT: $LUT\nFF: $FF\nDSP: $DSP\nBRAM: $BRAM\nCARRY8: $CARRY8\n"
  )

  def printFMax(): Unit = logger.info(s"\nfmax = ${Frequency / 1e6} MHz\n")

  def getReport = Array(
    LUT.toString,
    FF.toString,
    DSP.toString,
    BRAM.toString,
    URAM.toString,
    Frequency.toString
  )

  def getUtil = util

  override def toString: String =
    s"LUT $LUT, FF $FF, DSP $DSP, BRAM $BRAM, URAM $URAM, CARRY8 $CARRY8, Freq $Frequency"

  def require(utilRequirement: VivadoUtil, fmaxRequirement: HertzNumber): Unit = {
    assert(
      this.util <= utilRequirement,
      s"util failed: yours: $util, target: $utilRequirement"
    )
    assert(
      this.Frequency >= fmaxRequirement.toDouble,
      s"critical path failed: yours: ${Frequency / 1e6} MHz, target: $fmaxRequirement"
    )
  }

  log.close()
}
