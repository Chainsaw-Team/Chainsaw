package Chainsaw.io

import ai.djl.ndarray._
import spinal.core._

import java.io.{BufferedReader, File, InputStreamReader, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.implicitConversions

object pythonIo {

  val pythongProjectDir = new File("goldenModel")
  val pythonUtilsPath =
    new File(pythongProjectDir, "utils")
  val pythonPath =
    new File(sys.env.getOrElse("PYTHON", "")) // python executable dir

  // array io
  type Signal = Seq[BigDecimal] // datatype for golden model

  val inputArrayFile  = new File(pythonUtilsPath, "input.npz")
  val outputArrayFile = new File(pythonUtilsPath, "output.npz")
  val configFile      = new File(pythonUtilsPath, "config.json")

  /** export signals to a .npz file
   */
  def exportSignal(signals: Signal*): File = {
    val manager = NDManager.newBaseManager()
    val arrays  = signals.toArray.map(signal => manager.create(signal.toArray.map(_.toDouble)))
    val signal  = new NDList(arrays: _*)
    val file    = inputArrayFile
    val os      = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  // FIXME: better export method
  def exportSignal(file: File, yours: Signal*): File = {
    val manager = NDManager.newBaseManager()
    val arrays  = yours.toArray.map(signal => manager.create(signal.toArray.map(_.toDouble)))
    val signal  = new NDList(arrays: _*)
    val os      = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  def importSignal(file: File = outputArrayFile): Seq[Signal] = {
    val manager     = NDManager.newBaseManager()
    val is          = Files.newInputStream(file.toPath)
    val decoded     = NDList.decode(manager, is)
    val signalCount = decoded.size()
    val ret = (0 until signalCount)
      .map(decoded.get)
      .map(_.toDoubleArray.map(BigDecimal(_)).toSeq)
    ret
  }

  import org.json4s._
  import org.json4s.jackson.JsonMethods._
  import org.json4s.jackson.Serialization.{read, write}

  /** import serialized python dict for config parameters
    * @param file
    * @tparam T
    * @return
    */
  // tutorial: https://queirozf.com/entries/json4s-examples-common-basic-operations-using-jackson-as-backend
  def importConfig[T: Manifest](file: File = configFile) = {
    val src              = Source.fromFile(file)
    val lines            = src.getLines().mkString("\n")
    implicit val formats = DefaultFormats
    parse(lines).extract[T]
  }

  def exportConfig[T <: AnyRef](config: T) = {
    implicit val formats = DefaultFormats
    val lines            = write[T](config)
    val pw               = new PrintWriter(configFile)
    pw.write(lines)
    pw.close()
  }

  def runPython(pyPath: File, args: String*): String = {
    val command = s"$pythonPath ${pyPath.getAbsolutePath} ${args.mkString(" ")}"
    println(command)
    val process: Process = Runtime.getRuntime.exec(
      command,
      Array[String](),
      pythonUtilsPath
    ) // 执行py文件
    val in    = new BufferedReader(new InputStreamReader(process.getInputStream))
    val lines = ArrayBuffer[String]()

    var line = in.readLine()
    while (line != null) {
      lines += line
      line = in.readLine()
    }

    in.close()
    println(s"python output:\n\t${lines.mkString("\n\t")}")
    lines.lastOption.getOrElse("")
  }

  def goldenModelBySignal(
      pyPath: File,
      args: String,
      signal: Signal*
  ): Seq[Signal] = {
    exportSignal(signal: _*)
    runPython(pyPath, args)
    importSignal()
  }

  def main(args: Array[String]): Unit = {
    case class Config(
        name: String,
        pulse_points: Int,
        gauge_points: Int,
        data: Array[Int]
    )
    exportConfig(Config("config", 50, 50000, Array(1, 2, 3, 4)))
    println(importConfig[Config]().data.mkString(" "))
  }
}
