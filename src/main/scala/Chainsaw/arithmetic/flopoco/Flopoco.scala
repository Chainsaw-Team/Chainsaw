package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._

import java.io.File
import scala.io.Source

abstract class Flopoco extends ChainsawGenerator {

  // required parameters
  val operatorName: String
  val frequency: HertzNumber
  val family: XilinxDeviceFamily

  // optional parameters
  val params: Seq[(String, Any)]

  val outputDir = new File("src/main/resources/flopocoGenerated")

  def familyLine = family match {
    case UltraScale => "VirtexUltrascalePlus"
    case Series7 => "Kintex7"
  }

  override def name = {
    val paramsInName = (params :+ ("target", familyLine)).map { case (param, value) => s"${param}_$value" }.mkString("_")
    s"${operatorName}_$paramsInName"
  }

  /** output RTL file name defined by all params, making cache file possible
   */
  def rtlPath = new File(outputDir, s"$name.vhd")

  /** rtl model used in simulation
   */
  def model(dataIn: Seq[Bits]): Seq[Bits]

  /** black box used in synthesis
   */
  def blackbox: FlopocoBlackBox

  override def implH = new ChainsawModule(this) {
    val ret =
      if (atSimTime)
        model(dataIn.map(_.d(latency)))
      else {
        val box = blackbox
        box.addRTLPath(rtlPath.getAbsolutePath)
        box.definitionName = moduleName
        box.asFunc(dataIn)
      }
    dataOut := Vec(ret)
  }

  /** invoke flopoco to generate RTL and get terminal output
   */
  def flopocoRun(): Unit = {
    val paramsLine = params.map { case (param, value) => s"$param=$value" }.mkString(" ")
    val command = s"$flopocoPath frequency=${frequency.toInt / 1e6} target=$familyLine verbose=1 outputFile=${rtlPath.getAbsolutePath} ilpSolver=cplex $operatorName $paramsLine"
    outputDir.mkdirs()
    DoCmd.doCmd(command, outputDir.getAbsolutePath)
  }

  /** extract the module name as well as the pipeline level from generated RTL
   */
  def getInfoFromRtl: (Int, String) = {
    val src = Source.fromFile(rtlPath)
    val lines = src.getLines().toSeq
    val lineIndex = lines.indexWhere(_.contains(s"${operatorName}_"))
    val linesForSearch = lines.drop(lineIndex)
    val latency = linesForSearch
      .filter(_.startsWith("-- Pipeline depth: ")).head
      .filter(_.isDigit).mkString("").toInt
    val defName = linesForSearch
      .filter(_.startsWith(s"entity ${operatorName}_")).head
      .split(" ")(1)
    src.close()
    (latency, defName)
  }

  override var latency = 0
  var moduleName: String = ""

  def flopocoDone(): Unit = {
    if (!rtlPath.exists()) flopocoRun()
    val info = getInfoFromRtl
    logger.info(s"flopoco latency = $latency")
    latency = info._1
    logger.info(s"flopoco module name = $moduleName")
    moduleName = info._2
  }
}

abstract class FlopocoBlackBox() extends BlackBox {

  val clk = in Bool()
  mapCurrentClockDomain(clk)

  def asFunc: Seq[Bits] => Seq[Bits]
}