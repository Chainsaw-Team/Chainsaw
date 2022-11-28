package Chainsaw.intel

import Chainsaw.phases
import org.apache.commons.io.FileUtils
import spinal.core._

import java.io._
import java.nio.file.Paths
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._
import java.io.File

object Report extends Enumeration {
  type ReportType = Value
  val RESOURCE, TIMING = Value
}

class QuartusFlow[T <: Component](dut: => T = null, workspace: String = "quartusWorkspace", netlistDir: Option[File] = None) {

  val revisionName = "tempRef"
  val mapReportFile = revisionName + ".map.rpt"
  val staReportFile = revisionName + ".sta.rpt"

  val FAMILY = "Cyclone V"
  val DEVICE = "5CGXFC7C6U19A7"

  def impl(): Unit = {
    // new workspace
    s"mkdir $workspace".run()
    println("")
    //generate RTL`
    netlistDir match {
      case Some(file) => FileUtils.copyDirectory(file, new File(workspace))
      case None =>
        val config = SpinalConfig(targetDirectory = workspace)
        config.addTransformationPhase(new phases.FfIo)
        config.generateVerilog(dut.setDefinitionName("temp"))
    }

    // clear the workspace--shell
    Process("rm *txt *Ref* *qpf *qsf", new File(workspace))
    Process("rm -rf db inc*", new File(workspace)) !

    // new project and compile--tcl
    tclGen()
    Process("quartus_sh -t set.tcl", new File(workspace)) !
    // get report
    val resourceReport = getReport(Report.RESOURCE)
    val timingReport = getReport(Report.TIMING)
    // display report
    println(resourceReport.mkString("\n"))
    println(timingReport.mkString("\n"))
  }

  def tclGen(): Unit = {
    val laodFlow = "load_package flow\n"
    val newProject = "project_new -overwrite -r " + revisionName + " temp\n"
    val setTop = "set_global_assignment -name TOP_LEVEL_ENTITY temp\n"
    val setFamily = "set_global_assignment -name FAMILY \"" + FAMILY + "\"\n"
    val loadVerilogFile = "set_global_assignment -name VERILOG_FILE temp.v\n"
    val compileProject = "execute_flow -compile\n"
    val tclSeq = Seq(laodFlow, newProject, setTop, setFamily, loadVerilogFile, compileProject)

    val tclFile = new File(workspace + "/" + "set.tcl")
    val writer = new PrintWriter(tclFile)
    tclSeq.foreach(writer.write)
    writer.close()
  }

  def getReport(reportType: Report.ReportType): Array[String] = {
    try {
      var fileName = ""
      reportType match {
        case Report.RESOURCE => fileName = mapReportFile
        case Report.TIMING => fileName = staReportFile
      }
      val rptFile = Paths.get(workspace, fileName).toFile
      val report = Source.fromFile(rptFile).getLines().toArray
      reportType match {
        case Report.RESOURCE =>
          var index = -1
          report.zipWithIndex.foreach { case (r, i) => if (r.contains("Analysis & Synthesis Resource Usage Summary")) index = i }
          val resourceReport = report.zipWithIndex.filter { case (r, i) => i >= index - 1 && i <= index + 23 }.map(_._1)
          resourceReport

        case Report.TIMING =>
          var index = -1
          report.zipWithIndex.foreach { case (r, i) => if (r.contains("Slow 1100mV 85C Model Fmax Summary")) index = i }
          val timingReport = report.zipWithIndex.filter { case (r, i) => i >= index - 1 && i <= index + 6 }.map(_._1)
          timingReport
      }
    } catch {
      case e: FileNotFoundException =>
        println(e)
        Array("")
    }
  }
}

object QuartusFlow extends App {

  import Chainsaw._

  //  new QuartusFlow(Chainsaw.dsp.ComplexMult(SFixInfo(0, 17)).implH).impl()
  val netList = new File("/home/ltr/Chainsaw/src/main/resources/netlists")
  new QuartusFlow(netlistDir = Some(netList)).impl()
}
