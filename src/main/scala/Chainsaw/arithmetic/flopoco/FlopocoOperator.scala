package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim.SpinalSimBackendSel
import spinal.core.sim.SpinalSimBackendSel.{GHDL, VERILATOR}
import spinal.lib._

import java.io.File
import scala.io.Source

trait Flopoco {

  /** -------- params for Flopoco generation
    * --------
    */
  // required parameters
  def name: String

  val operatorName: String

  def entityName: String = operatorName

  val family: XilinxDeviceFamily

  def fmaxEstimation: HertzNumber

  private def familyLine = family match {
    case UltraScale => "VirtexUltrascalePlus"
    case Series7    => "Kintex7"
  }

  // optional parameters
  val params: Seq[(String, Any)]

  def getSimBackEnd: SpinalSimBackendSel =
    if (testFlopoco) GHDL else VERILATOR // for VHDL verification

  def getUseNaive: Boolean =
    if (atSimTime) !testFlopoco // while sim
    else false                  // while synth/impl

  /** output RTL file name defined by all params, making cache file possible
    */
  def rtlPath = new File(flopocoOutputDir, s"$name.vhd")

  /** black box used in synthesis
    */
  def blackbox: FlopocoBlackBox

  /** invoke flopoco to generate RTL and get terminal output
    */
  def flopocoRun(): Unit = {
    val paramsLine =
      params.map { case (param, value) => s"$param=$value" }.mkString(" ")
    val command =
      s"$flopocoPath frequency=${fmaxEstimation.toInt / 1e6} target=$familyLine verbose=1 outputFile=${rtlPath.getAbsolutePath} $operatorName $paramsLine"
    flopocoOutputDir.mkdirs()
    DoCmd.doCmd(command, flopocoOutputDir.getAbsolutePath)
  }

  /** extract the module name as well as the pipeline level from generated RTL
    */
  def getInfoFromRtl: (Int, String) = { // FIXME: this is not robust
    if (!rtlPath.exists()) flopocoRun()
    val src            = Source.fromFile(rtlPath)
    val lines          = src.getLines().toSeq
    val lineIndex      = lines.indexWhere(_.contains(s"${entityName}_"))
    val linesForSearch = lines.drop(lineIndex)
    val latencyLine = linesForSearch.find(_.startsWith("-- Pipeline depth: "))
    val latency = latencyLine match {
      case Some(line) => line.filter(_.isDigit).mkString("").toInt
      case None       => 0
    }
    //    println(linesForSearch.mkString("\n"))
    val moduleName = linesForSearch
      .filter(_.startsWith(s"entity ${entityName}_"))
      .head
      .split(" ")(1)
    src.close()
    (latency, moduleName)
  }

  def latency() = getInfoFromRtl._1

  def moduleName: String = getInfoFromRtl._2
}

/** base class of wrappers of Flopoco operators
  */
abstract class FlopocoOperator extends ChainsawOperatorGenerator with Flopoco {

  override def simBackEnd: SpinalSimBackendSel = getSimBackEnd

  override def useNaive = getUseNaive

  override def implH = new ChainsawOperatorModule(this) {
    val box = blackbox
    box.addRTLPath(rtlPath.getAbsolutePath)
    println(s"moduleName = $moduleName")
    box.setDefinitionName(moduleName)
    box.mapChainsawModule(flowIn, flowOut)
  }

  override def doSelfTest() = {
    testFlopoco = true
    super.doSelfTest()
  }
}

abstract class FlopocoBlackBox extends BlackBox {
  def mapChainsawModule(
      flowIn: Flow[Fragment[Vec[AFix]]],
      flowOut: Flow[Fragment[Vec[AFix]]]
  ): Unit
}

abstract class FlopocoBlackBoxWithClk extends FlopocoBlackBox {
  val clk = in Bool ()
  mapCurrentClockDomain(clk)
}
