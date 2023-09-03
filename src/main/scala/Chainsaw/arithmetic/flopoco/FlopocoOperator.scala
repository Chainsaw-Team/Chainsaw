package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow._
import spinal.core._

import java.io.File
import scala.io.Source
import scala.language.postfixOps

/** for implementing of FloPoCo operators as Chainsaw operators, which can be verified and synthesized automatically
 */
abstract class FlopocoOperator(override val family: XilinxDeviceFamily, override val targetFrequency: HertzNumber)
  extends Flopoco(family, targetFrequency)
    with ChainsawOperatorGenerator with FixedLatency {

  implicit val flopocoGen: FlopocoOperator = this

  // template of def implH in FlopocoOperator
  //  new ChainsawOperatorModule(this) {
  //      val box = new FlopocoBlackBox(hasClk = ) {
  //      // setting I/O for black box
  //      }
  //      // mapping I/O of ChainsawOperatorModule to the black box
  //    }

  override def fmaxEstimation: HertzNumber = targetFrequency
}

/** black box designed for FlopocoOperator implementation, which addRtlPath and setDefinitionName automatically
 *
 * @param hasClk  whether the black box has a clock input named "clk", when true, define the clock input and map it to the current clock domain
 * @param flopoco background FlopocoOperator
 */
abstract class FlopocoBlackBox(hasClk: Boolean)(implicit val flopoco: Flopoco) extends BlackBox {
  hasClk.generate {
    val clk = in Bool()
    clk.setName("clk")
    mapCurrentClockDomain(clk)
  }
  addRTLPath(flopoco.verilogFile.getAbsolutePath)
  logger.info(s"setting${flopoco.moduleName}")
  setDefinitionName(flopoco.moduleName)
}

/** providing generation methods for FloPoCo operators
 *
 * @param family FPGA device family, currently supporting Series7, UltraScale and UltraScalePlus
 */
abstract class Flopoco(val family: XilinxDeviceFamily, val targetFrequency: HertzNumber) {

  /** -------- params for FloPoCo generation
   * --------
   */

  //
  val operatorName: String

  // prefix of the generated module name
  val entityName: String

  val params: Seq[(String, Any)]

  // this should be a valid verilog/vhdl module name, which is unique
  def name: String = s"${operatorName}_${params.map { case (param, value) => s"${param}_$value" }.mkString("_")}"

  private def familyLine = family match {
    case UltraScalePlus => "VirtexUltrascalePlus"
    case UltraScale => "VirtexUltrascalePlus"
    case Series7 => "Kintex7"
    case device => logger.warn(s"$device not supported, using Kintex7 as default")
      "Kintex7"
  }

  def vdhFile: File = {
    val ret = new File(flopocoOutputDir, s"$name.vhd")
    if (!ret.exists()) {
      val optionsLine = s"frequency=${targetFrequency.toInt / 1e6} target=$familyLine verbose=1 outputFile=${ret.getAbsolutePath}"
      val paramsLine = params.map { case (param, value) => s"$param=$value" }.mkString(" ")
      doCmd(s"$flopocoPath $optionsLine $operatorName $paramsLine")
    }
    ret
  }

  def verilogFile: File = {
    val ret = new File(flopocoOutputDir, s"$name.v")
    if (!ret.exists()) {
      doCmds(Seq(
        s"ghdl -a -fsynopsys -fexplicit ${vdhFile.getAbsolutePath}",
        s"ghdl synth -fsynopsys -fexplicit --out=verilog $moduleName > ${ret.getAbsolutePath}"))
    }
    ret
  }

  /** find latency and top module name from generated .vhd file
   */
  def getInfoFromRtl: (Int, String) = { // TODO: better method
    val src = Source.fromFile(vdhFile)
    val lines = src.getLines().toSeq
    val lineIndex = lines.indexWhere(_.contains(s"${entityName}_"))
    val linesForSearch = lines.drop(lineIndex)
    val latencyLine = linesForSearch.find(_.startsWith("-- Pipeline depth: "))
    val latency = latencyLine match {
      case Some(line) => line.filter(_.isDigit).mkString("").toInt
      case None => 0
    }
    //    println(linesForSearch.mkString("\n"))
    val moduleName = linesForSearch
      .filter(_.startsWith(s"entity ${entityName}_"))
      .head
      .split(" ")(1)
    src.close()
    println(s"latency = $latency, moduleName = $moduleName")
    (latency, moduleName)
  }

  lazy val latency: Int = getInfoFromRtl._1

  lazy val moduleName: String = getInfoFromRtl._2
}