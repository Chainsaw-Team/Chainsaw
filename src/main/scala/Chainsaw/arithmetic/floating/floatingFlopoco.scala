package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.{Series7, UltraScale, UltraScalePlus, XilinxDeviceFamily}
import Chainsaw.xilinx.XilinxDevice
import Chainsaw.{ChainsawBaseGenerator, FLOPOCO, doCmd, doCmds, logger}
import spinal.core.sim.SimConfig
import spinal.core.{HertzNumber, IntToBuilder}

import java.io.File
import scala.io.Source

trait floatingFlopoco {

  val operatorName: String

  val entityName: String

  val params: Seq[(String, Any)]

  val family: XilinxDeviceFamily

  val targetFrequency: HertzNumber

  // this should be a valid verilog/vhdl module name, which is unique
  private def name: String = {
    var nameRaw =  s"${operatorName}_F${targetFrequency.toInt/1e6.toInt}_${params.map { case (param, value) => s"${param}_$value"}.mkString("_")}"
    nameRaw = nameRaw.replace('-', 'N')
    nameRaw = nameRaw.replace('+', 'P')
    nameRaw = nameRaw.replace('.', 'p')
    nameRaw = nameRaw.replace('(', '_')
    nameRaw = nameRaw.replace(')', '_')
    nameRaw = nameRaw.replace("\"", "")
    nameRaw = nameRaw.replace("\'", "")
    nameRaw
  }

  private def familyLine = family match {
    case UltraScalePlus => "VirtexUltrascalePlus"
    case UltraScale => "VirtexUltrascalePlus"
    case Series7 => "Kintex7"
    case device => logger.warn(s"$device not supported, using Kintex7 as default")
      "Kintex7"
  }

  def vhdFile: File = {
    val ret = new File(FLOPOCO.workspace, s"$name.vhd")
    if (!ret.exists()) {
      val optionsLine = s"frequency=${targetFrequency.toInt / 1e6} target=$familyLine verbose=1 outputFile=${ret.getAbsolutePath}"
      val paramsLine = params.map { case (param, value) => s"$param=$value" }.mkString(" ")
      doCmd(s"${FLOPOCO.path} $optionsLine $operatorName $paramsLine")
    }
    ret
  }

  def verilogFile: File = {
    val ret = new File(FLOPOCO.workspace, s"$name.v")
    if (!ret.exists()) {
      doCmds(Seq(
        s"ghdl -a -fsynopsys -fexplicit ${vhdFile.getAbsolutePath}",
        s"ghdl synth -fsynopsys -fexplicit --out=verilog $moduleName > ${ret.getAbsolutePath}"))
    }
    ret
  }

  /** find latency and top module name from generated .vhd file
    */
  def getInfoFromRtl: (Int, String) = { // TODO: better method
    val src = Source.fromFile(vhdFile)
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
