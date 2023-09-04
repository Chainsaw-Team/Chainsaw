package Chainsaw.io

import ai.djl.ndarray._
import Chainsaw._

import java.io.{BufferedReader, File, InputStreamReader, PrintWriter}
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.implicitConversions

object pythonIo {

  // array io
  val pythongProjectDir = new File("goldenModel")
  val pythonDataPath    = new File(pythongProjectDir, "data")
  val pythonIoPath      = new File(pythongProjectDir, "PythonIo.py")

  val inputArrayFile   = new File(pythonDataPath, "input.npz")
  val inputControlFile = new File(pythonDataPath, "control.npz")
  val outputArrayFile  = new File(pythonDataPath, "output.npz")
  val configFile       = new File(pythonDataPath, "config.json")

  /** export signals to a .npz file
    */
  def exportSignals(file: File, signals: Signal*): File = {
    val manager = NDManager.newBaseManager()
    val arrays  = signals.toArray.map(signal => manager.create(signal.toArray.map(_.toDouble)))
    val signal  = new NDList(arrays: _*)
    val os      = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  def exportSignal(file: File, signal: Signal): File = exportSignals(file, signal)

  def importSignals(file: File): Seq[Signal] = {
    val manager     = NDManager.newBaseManager()
    val is          = Files.newInputStream(file.toPath)
    val decoded     = NDList.decode(manager, is)
    val signalCount = decoded.size()
    (0 until signalCount)
      .map(decoded.get)
      .map(_.toDoubleArray.map(BigDecimal(_)).toSeq)
  }
  def importSignal(file: File): Signal = importSignals(file).head

  import org.json4s._
  import org.json4s.jackson.JsonMethods._
  import org.json4s.jackson.Serialization.write

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

  // end-to-end python golden model
  def runPython(workingDirectory: File, args: String*): String = {
    require(PYTHON.exist(), "to use python module, please set the environment variable 'PYTHON' to the python executable with numpy,scipy & matplotlib, e.g. /usr/bin/python3")
    val command = s"${PYTHON.path} ${workingDirectory.getAbsolutePath} ${args.mkString(" ")}"
    println(command)
    val process: Process = Runtime.getRuntime.exec(
      command,
      Array[String](),
      pythongProjectDir
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

  def runPythonModel(
      moduleName: String,
      functionName: String,
      data: Signal,
      control: Option[Signal] = None,
      config: Option[AnyRef]  = None
  ): Signal = {
    exportSignal(inputArrayFile, data)
    control match {
      case Some(value) => exportSignal(inputControlFile, value)
      case None        =>
    }
    config match {
      case Some(value) => exportConfig(value)
      case None        =>
    }
    runPython(pythonIoPath, moduleName, functionName)
    importSignal(outputArrayFile)
  }

  case class ExampleConfig(a: Seq[Double] = Seq(1, 2, 3), b: Seq[Double] = Seq(4, 5, 6))

  def main(args: Array[String]): Unit = {
    val data   = Seq(1, 2, 3).map(BigDecimal(_))
    val config = ExampleConfig()
    val ret    = runPythonModel("dsp", "lfilter", data, None, Some(config))
    println(ret.mkString(" "))
  }
}
