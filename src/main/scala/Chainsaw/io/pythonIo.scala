package Chainsaw.io

import ai.djl.ndarray._
import Chainsaw._
import ai.djl.ndarray.types.Shape

import java.io.{BufferedReader, DataInputStream, File, InputStreamReader, PrintWriter}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.regex.Pattern
import java.util.zip.{ZipEntry, ZipInputStream}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.implicitConversions

object pythonIo {

  // io paths
  val pythonProjectDir = new File("goldenModel")
  val pythonDataPath    = new File(pythonProjectDir, "data")
  val pythonIoPath      = new File(pythonProjectDir, "PythonIo.py")

  val inputArrayFile   = new File(pythonDataPath, "input.npz")
  val inputControlFile = new File(pythonDataPath, "control.npz")
  val outputArrayFile  = new File(pythonDataPath, "output.npz")
  val configFile       = new File(pythonDataPath, "config.json")

  /** export BigDecimal signals to a .npz file
   *  caution: this function convert BigDecimal to Double,
   *  might cost too much time when signal is long
  */
  def exportSignals(file: File, signals: Signal*): File = {
    val manager = NDManager.newBaseManager()
    val arrays  = signals.toArray.map(signal => manager.create(signal.toArray.map(_.toDouble)))
    val signal  = new NDList(arrays: _*)
    val os      = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  /** export named BigDecimal signals to a .npz file
   *  caution: this function convert BigDecimal to Double,
   *  might cost too much time when signal is long
   */
  def exportSignals(file: File, signals: Map[String, Seq[BigDecimal]]): File = {
    val manager = NDManager.newBaseManager()
    val arrays  = signals.toArray.map(signal => {
      val t = manager.create(signal._2.toArray.map(_.toDouble))
      t.setName(signal._1)
      t
    })
    val signal  = new NDList(arrays: _*)
    val os      = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  /** export Double signals to a .npz file
   */
  def exportSignalsDouble(file: File, signals: Seq[Double]*): File = {
    val manager = NDManager.newBaseManager()
    val arrays  = signals.toArray.map(signal => manager.create(signal.toArray))
    val signal  = new NDList(arrays: _*)
    val os      = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  /** export named Double signals to a .npz file
   */
  def exportSignalsDouble(file: File, signals: Map[String, Seq[Double]]): File = {
    val manager = NDManager.newBaseManager()
    val arrays  = signals.toArray.map(signal => {
      val t = manager.create(signal._2.toArray)
      t.setName(signal._1)
      t
    })
    val signal  = new NDList(arrays: _*)
    val os      = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  /** export named BigDecimal 2d signals to a .npz file
   *  caution: this function convert BigDecimal to Double,
   *  might cost too much time when signal is long
   */
  def exportSignals2d(file: File, signals: Map[String, Seq[Signal]]): File = {
    val manager = NDManager.newBaseManager()
    val arrays  = signals.toArray.map(signal => {
      val t = manager.create(signal._2.toArray.map(_.toArray.map(_.toDouble)))
      t.setName(signal._1)
      t
    })
    val signal  = new NDList(arrays: _*)
    val os      = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  /** export named Double 2d signals to a .npz file
   */
  def exportSignalsDouble2d(file: File, signals: Map[String, Seq[Seq[Double]]]): File = {
    val manager = NDManager.newBaseManager()
    val arrays  = signals.toArray.map(signal => {
      val t = manager.create(signal._2.toArray.map(_.toArray))
      t.setName(signal._1)
      t
    })
    val signal  = new NDList(arrays: _*)
    val os      = Files.newOutputStream(file.toPath)
    signal.encode(os, true)
    file
  }

  /** convert named BigDecimal 2d signals to series of signals
   *  converted signal name: {name}_2d_i.npz
   */
  def flatten2dSignals(signals: Map[String, Seq[Signal]]): Map[String, Signal] = {
    var signal2d: Map[String, Signal] = Map()
    signals.foreach(signal => {
      signal._2.zipWithIndex.foreach(s => {
        signal2d = signal2d + (signal._1+s"_2d_${s._2}" -> s._1)
      })
    })
    signal2d
  }

  /** convert named Double 2d signals to series of signals
   *  converted signal name: name_2d_i
   */
  def flatten2dSignalsDouble(signals: Map[String, Seq[Seq[Double]]]): Map[String, Seq[Double]] = {
    var signal2d: Map[String, Seq[Double]] = Map()
    signals.foreach(signal => {
      signal._2.zipWithIndex.foreach(s => {
        signal2d = signal2d + (signal._1+s"_2d_${s._2}" -> s._1)
      })
    })
    signal2d
  }

  /** export one BigDecimal signal
   *  caution: this function convert BigDecimal to Double,
   *  might cost too much time when signal is long
   */
  def exportSignal(file: File, signal: Signal): File = exportSignals(file, signal)

  def exportSignalDouble(file: File, signal: Seq[Double]): File = exportSignalsDouble(file, signal)

  /** calc imported
   */
  def getSignalSizeBigDecimal(file: File): Long = {
    val is = Files.newInputStream(file.toPath)
    val zis = new ZipInputStream(is)
    var entry: ZipEntry = zis.getNextEntry
    var sizeBigDecimal = 0L

    while (entry != null) {
      val NUMPY_MAGIC = Array(0x93.toByte, 'N', 'U', 'M', 'P', 'Y')
      val PATTERN = Pattern.compile("\\{'descr': '(.+)', 'fortran_order': False, 'shape': \\((.*)\\),")

      // check npz header
      val ds = new DataInputStream(zis)
      var buf = new Array[Byte](NUMPY_MAGIC.length)
      ds.readFully(buf)
      if (! buf.sameElements(NUMPY_MAGIC)) throw new IllegalArgumentException("Malformed numpy data")
      val major = ds.readByte
      val minor = ds.readByte
      if (major < 1 || major > 3 || minor != 0) throw new IllegalArgumentException("Unknown numpy version: " + major + '.' + minor)

      // get npz header lenth
      var len = if (major == 1) 2 else 4
      ds.readFully(buf, 0, len)
      val bb = ByteBuffer.wrap(buf, 0, len)
      bb.order(ByteOrder.LITTLE_ENDIAN)
      if (major == 1) len = bb.getShort
      else len = bb.getInt

      // parse npz header to get shape
      buf = new Array[Byte](len)
      ds.readFully(buf)
      val header = new String(buf, StandardCharsets.UTF_8).trim
      val m = PATTERN.matcher(header)
      if (!m.find) throw new IllegalArgumentException("Invalid numpy header: " + header)
      val shapeStr = m.group(2)
      var longs = Array(0L)
      if (shapeStr.isEmpty) {}
      else {
        val tokens = shapeStr.split(", ?")
        longs = tokens.map(_.toLong)
      }
      val shape = new Shape(longs:_*)

      // calc array size, 8 for Double, (8 + 4 + 4 + 104) for BigDecimal
      sizeBigDecimal += shape.getShape.foldLeft(1L)((a, b) => a * b) * (8 + 4 + 4 + 104)

      // get next file
      entry = zis.getNextEntry
    }
    sizeBigDecimal
  }

  def getSignalSizeDouble(file: File): Long = {
    val is = Files.newInputStream(file.toPath)
    val zis = new ZipInputStream(is)
    var entry: ZipEntry = zis.getNextEntry
    var sizeBigDecimal = 0L

    while (entry != null) {
      val NUMPY_MAGIC = Array(0x93.toByte, 'N', 'U', 'M', 'P', 'Y')
      val PATTERN = Pattern.compile("\\{'descr': '(.+)', 'fortran_order': False, 'shape': \\((.*)\\),")

      // check npz header
      val ds = new DataInputStream(zis)
      var buf = new Array[Byte](NUMPY_MAGIC.length)
      ds.readFully(buf)
      if (! buf.sameElements(NUMPY_MAGIC)) throw new IllegalArgumentException("Malformed numpy data")
      val major = ds.readByte
      val minor = ds.readByte
      if (major < 1 || major > 3 || minor != 0) throw new IllegalArgumentException("Unknown numpy version: " + major + '.' + minor)

      // get npz header length
      var len = if (major == 1) 2 else 4
      ds.readFully(buf, 0, len)
      val bb = ByteBuffer.wrap(buf, 0, len)
      bb.order(ByteOrder.LITTLE_ENDIAN)
      if (major == 1) len = bb.getShort
      else len = bb.getInt

      // parse npz header to get shape
      buf = new Array[Byte](len)
      ds.readFully(buf)
      val header = new String(buf, StandardCharsets.UTF_8).trim
      val m = PATTERN.matcher(header)
      if (!m.find) throw new IllegalArgumentException("Invalid numpy header: " + header)
      val shapeStr = m.group(2)
      var longs = Array(0L)
      if (shapeStr.isEmpty) {}
      else {
        val tokens = shapeStr.split(", ?")
        longs = tokens.map(_.toLong)
      }
      val shape = new Shape(longs:_*)

      // estimate array size, 8 for Double, (8 + 4 + 4 + 104) for BigDecimal
      sizeBigDecimal += shape.getShape.foldLeft(1L)((a, b) => a * b) * (8 + 4 + 4 + 104)

      // get next file
      entry = zis.getNextEntry
    }
    sizeBigDecimal
  }

  /** import BigDecimal signals form npz file
   *  caution: this function convert BigDecimal to Double,
   *  might cost too much time when signal is long
   */
  def importSignals(file: File): Seq[Signal] = {
    val allocatedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val freeMemory = Runtime.getRuntime.maxMemory() - allocatedMemory
    val neededMemory = getSignalSizeDouble(file)

    assert(freeMemory * 0.9 > neededMemory, s"Warning: not enough memory to import signal, needed: $neededMemory, available: $freeMemory")

    val manager     = NDManager.newBaseManager()
    val is          = Files.newInputStream(file.toPath)
    val decoded     = NDList.decode(manager, is)
    val signalCount = decoded.size()
    (0 until signalCount)
      .map(decoded.get)
      .map(_.toDoubleArray.map(BigDecimal.valueOf).toSeq)
  }

  /** import named Double signals form npz file
   *  caution: this function convert BigDecimal to Double,
   *  might cost too much time when signal is long
   */
  def importSignalsNamed(file: File): Map[String, Signal] = {
    val allocatedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val freeMemory = Runtime.getRuntime.maxMemory() - allocatedMemory
    val neededMemory = getSignalSizeDouble(file)

    assert(freeMemory * 0.9 > neededMemory, s"Warning: not enough memory to import signal, needed: $neededMemory, available: $freeMemory")

    val manager     = NDManager.newBaseManager()
    val is          = Files.newInputStream(file.toPath)
    val decoded     = NDList.decode(manager, is)
    val signalCount = decoded.size()
    (0 until signalCount)
      .map(decoded.get)
      .map(arr => {
        arr.getName.replace(".npy", "") -> arr.toDoubleArray.map(BigDecimal.valueOf).toSeq
      })
      .toMap
  }

  /** import Double signals form npz file
   */
  def importSignalsDouble(file: File): Seq[Seq[Double]] = {
    val allocatedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val freeMemory = Runtime.getRuntime.maxMemory() - allocatedMemory
    val neededMemory = getSignalSizeDouble(file)

    assert(freeMemory * 0.9 > neededMemory, s"Warning: not enough memory to import signal, needed: $neededMemory, available: $freeMemory")

    val manager     = NDManager.newBaseManager()
    val is          = Files.newInputStream(file.toPath)
    val decoded     = NDList.decode(manager, is)
    val signalCount = decoded.size()
    (0 until signalCount)
      .map(decoded.get)
      .map(_.toDoubleArray.toSeq)
  }

  /** import named Double signals form npz file
   */
  def importSignalsNamedDouble(file: File): Map[String, Seq[Double]] = {
    val allocatedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val freeMemory = Runtime.getRuntime.maxMemory() - allocatedMemory
    val neededMemory = getSignalSizeDouble(file)

    assert(freeMemory * 0.9 > neededMemory, s"Warning: not enough memory to import signal, needed: $neededMemory, available: $freeMemory")

    val manager     = NDManager.newBaseManager()
    val is          = Files.newInputStream(file.toPath)
    val decoded     = NDList.decode(manager, is)
    val signalCount = decoded.size()
    (0 until signalCount)
      .map(decoded.get)
      .map(arr => arr.getName.replace(".npy", "") -> arr.toDoubleArray.toSeq)
      .toMap
  }

  /** import 2d BigDecimal signals form npz file
   *  caution: this function convert BigDecimal to Double,
   *  might cost too much time when signal is long
   */
  def importSignals2d(file: File, shape: (Int, Int)): Map[String, Seq[Signal]] = {
    val allocatedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val freeMemory = Runtime.getRuntime.maxMemory() - allocatedMemory
    val neededMemory = getSignalSizeDouble(file)

    assert(freeMemory * 0.9 > neededMemory, s"Warning: not enough memory to import signal, needed: $neededMemory, available: $freeMemory")

    val manager     = NDManager.newBaseManager()
    val is          = Files.newInputStream(file.toPath)
    val decoded     = NDList.decode(manager, is)
    val signalCount = decoded.size()
    (0 until signalCount)
      .map(decoded.get)
      .map(arr => {
        val arrDouble = arr.toDoubleArray
        val name = arr.getName.replace(".npy", "")
        val data: ArrayBuffer[Seq[BigDecimal]] = ArrayBuffer()
        (0 to shape._1).foreach(i => {
          data.append(arrDouble.slice(i * shape._2, i * shape._2 + shape._2).map(BigDecimal.valueOf))
        })
        name -> data
      }).toMap

  }

  /** import 2d Double signals form npz file
   */
  def importSignalsDouble2d(file: File, shape: (Int, Int)): Map[String, Seq[Seq[Double]]] = {
    val allocatedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val freeMemory = Runtime.getRuntime.maxMemory() - allocatedMemory
    val neededMemory = getSignalSizeDouble(file)

    assert(freeMemory * 0.9 > neededMemory, s"Warning: not enough memory to import signal, needed: $neededMemory, available: $freeMemory")

    val manager     = NDManager.newBaseManager()
    val is          = Files.newInputStream(file.toPath)
    val decoded     = NDList.decode(manager, is)
    val signalCount = decoded.size()
    (0 until signalCount)
      .map(decoded.get)
      .map(arr => {
        val arrDouble = arr.toDoubleArray
        val name = arr.getName.replace(".npy", "")
        val data: ArrayBuffer[Seq[Double]] = ArrayBuffer()
        (0 until shape._1).foreach(i => {
          data.append(arrDouble.slice(i * shape._2, i * shape._2 + shape._2))
        })
        name -> data
      }).toMap

  }

  /** convert a series of 1d BigDecimal signals to named 2d signals
   */
  def fold2dSignals(signals: Map[String, Signal]): Map[String, Seq[Signal]] = {
    var ret: Map[String, Seq[Signal]] = Map()
    val signals2d = signals.filter(_._1.contains("_2d_"))
    val nameSet = signals2d.keys.map(_.split("_2d_").head).toSet
    nameSet.foreach(name => {
      val tmp = signals2d
        .filter(_._1.contains(name))
        .toArray
        .sortBy(s => s._1.split("_2d_").last.toInt)
        .map(_._2).toSeq
      ret = ret + (name -> tmp)
    })
    ret
  }

  /** convert a series of 1d Double signals to named 2d signals
   */
  def fold2dSignalsDouble(signals: Map[String, Seq[Double]]): Map[String, Seq[Seq[Double]]] = {
    var ret: Map[String, Seq[Seq[Double]]] = Map()
    val signals2d = signals.filter(_._1.contains("_2d_"))
    val nameSet = signals2d.keys.map(_.split("_2d_").head).toSet
    nameSet.foreach(name => {
      val tmp = signals2d
        .filter(_._1.contains(name))
        .toArray
        .sortBy(s => s._1.split("_2d_").last.toInt)
        .map(_._2).toSeq
      ret = ret + (name -> tmp)
    })
    ret
  }

  /** import one BigDecimal signal
   *  caution: this function convert Double to BigDecimal,
   *  might cost too much time when signal is long
   */
  def importSignal(file: File): Signal = importSignals(file).head

  /** import one Double signal
   *  caution: this function convert Double to BigDecimal,
   *  might cost too much time when signal is long
   */
  def importSignalDouble(file: File): Seq[Double] = importSignalsDouble(file).head

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
      pythonProjectDir
    ) // run python script
    val in    = new BufferedReader(new InputStreamReader(process.getInputStream))
    val err   = new BufferedReader(new InputStreamReader(process.getErrorStream))
    val lines = ArrayBuffer[String]()

    var line = in.readLine()
    while (line != null) {
      lines += line
      line = in.readLine()
    }

    line = err.readLine()
    while (line != null) {
      lines += line
      line = err.readLine()
    }

    in.close()
    err.close()
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
