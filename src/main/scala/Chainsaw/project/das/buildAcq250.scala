package Chainsaw.project.das

import org.apache.commons.io.FileUtils

import java.io.File
import java.nio.file.{Files, Paths}
import scala.sys.process._
import scala.io.StdIn.readLine

object buildAcq250 extends App {

  println("make sure you generate latest top module before running this script")

  ////////////////////
  // constructing paths
  ////////////////////
  val versionName = readLine("input version name\n")

  // user-define paths, may change from host to host
  val project_dir = "C:\\Users\\ltr\\Documents\\GitHub\\Chainsaw" // your own project directory
  assert(Files.exists(Paths.get(project_dir)), "project directory does not exist")
  val quartus_dir = "C:\\intelFPGA_lite\\23.1std\\quartus\\bin64"
  assert(Files.exists(Paths.get(quartus_dir)), "quartus directory does not exist")

  // paths derived
  val quartus         = s"$quartus_dir\\quartus.exe"
  val quartus_sh      = s"$quartus_dir\\quartus_sh.exe"
  val quartus_stp     = s"$quartus_dir\\quartus_stp.exe"
  val workspace       = s"$project_dir\\synthWorkspace\\$versionName"
  val source_dir      = project_dir
  val xillybus_ip_dir = s"$project_dir\\src\\main\\resources\\ips\\xillybus\\cycloneV"
  val board_dir       = s"$project_dir\\src\\main\\resources\\boards\\das"
  val board_ip_dir    = s"$board_dir\\ips"
  val tcl_script_path = s"$project_dir\\src\\main\\resources\\boards\\das\\createAcq250.tcl"

  ////////////////////
  // processing files
  ////////////////////
  val workspaceDir = new File(workspace)

  if (Files.exists(Paths.get(workspace))) {
    FileUtils.deleteDirectory(workspaceDir)
  }
  FileUtils.forceMkdir(workspaceDir)

  val srcs = Seq( // copy source files
    new File(source_dir, "Acq250Top.v"), // Top module
    new File(xillybus_ip_dir), // xillybus ip
    new File(board_ip_dir), // LVDS ip
    new File(board_dir, "Acq250.sdc"), // timing constraint
  ).foreach{file =>
    if (file.isDirectory) FileUtils.copyDirectory(file, workspaceDir)
    else FileUtils.copyFileToDirectory(file, workspaceDir)
  }

  ////////////////////
  // quartus tasks
  ////////////////////
  // Run the TCL script to create a Quartus project
  println(s"$quartus_sh -t $tcl_script_path")
  println(Process(s"$quartus_sh -t $tcl_script_path", workspaceDir).!!)

  val linkStp = readLine("link stp file?(Y/N)\n")
  if (linkStp == "Y") {
    println(
      Process(s"$quartus_stp Acq250Top --enable --signaltap --stp_file=$board_dir/Acq250.stp", workspaceDir).!!
    )
  }

  val task = readLine("project ready, open project(O) or compile project(C)\n")
  if (task == "O") {
    println(Process(s"$quartus Acq250Top.qpf", workspaceDir).!!) // Open the project created
  } else {
    println(Process(s"$quartus_sh --flow compile Acq250Top", workspaceDir).!!)
  }

  println(s"view output files in $workspace\\output_files")
}
