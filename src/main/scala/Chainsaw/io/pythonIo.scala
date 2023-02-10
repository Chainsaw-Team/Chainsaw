package Chainsaw.io

import ai.djl.ndarray._
import spinal.core._

import java.io.{BufferedReader, File, InputStreamReader}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object pythonIo {

  val pythongProjectDir = new File("goldenModel")
  val pythonPath =
    new File(sys.env.getOrElse("PYTHON", "")) // python executable dir

  // array io
  type Signal = Seq[BigDecimal] // datatype for golden model
  val arrayFile = new File(pythongProjectDir, "temp.npz")
  def exportSignal(yours: Signal*): File = {
    val manager = NDManager.newBaseManager()
    val arrays = yours.toArray.map(signal =>
      manager.create(signal.toArray.map(_.toDouble))
    )
    val signal = new NDList(arrays: _*)
    val file   = arrayFile
    val os     = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  def importSignal: Seq[Signal] = {
    val manager     = NDManager.newBaseManager()
    val is          = Files.newInputStream(arrayFile.toPath)
    val decoded     = NDList.decode(manager, is)
    val signalCount = decoded.size()
    (0 until signalCount)
      .map(decoded.get)
      .map(_.toDoubleArray.map(BigDecimal(_)).toSeq)
  }

  def runPython(pyPath: File, args: String*): String = {
    val command = s"$pythonPath ${pyPath.getAbsolutePath} ${args.mkString(" ")}"
    println(command)
    val process: Process = Runtime.getRuntime.exec(
      command,
      Array[String](),
      pythongProjectDir
    ) // 执行py文件
    val in = new BufferedReader(new InputStreamReader(process.getInputStream))
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

  def goldenModelBySignal(pyPath: File, args:String, signal: Signal*): Seq[Signal] = {
    exportSignal(signal: _*)
    runPython(pyPath, args)
    importSignal
  }

}
