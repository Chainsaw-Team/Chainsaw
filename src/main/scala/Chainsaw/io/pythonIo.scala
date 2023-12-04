package Chainsaw.io

import Chainsaw._
import ai.djl.ndarray._
import ai.djl.ndarray.types.Shape

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.{ByteBuffer, ByteOrder}
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

  /**
   * calculate memory requirement to import npz file as BigDecimal
   * @param file target file
   * @return size in Byte
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

  /**
   * calculate memory requirement to import npz file as Double
   * @param file target file
   * @return size in Byte
   */
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

  /**
   * export signals to a npz file
   * @param file target file
   */
  case class pythonExporter(file: File = inputArrayFile) {

    private val manager = NDManager.newBaseManager()
    private val list  = new NDList()

    /**
     * add BigDecimal signals to export
     *
     * caution: this function convert BigDecimal to Double,
     * might cost too much time when signal is long
     */
    def add(signals: Signal*) = {
      signals.toArray.foreach(signal => {
        list.add(manager.create(signal.toArray.map(_.toDouble)))
      })
      this
    }

    /**
     * add named BigDecimal Signals to export
     *
     * caution: this function convert BigDecimal to Double,
     * might cost too much time when signal is long
     *
     * @param signals map of name to signal
     */
    def add(signals: Map[String, Signal]) = {
      signals.toArray.foreach(signal => {
        val t = manager.create(signal._2.toArray.map(_.toDouble))
        t.setName(signal._1)
        list.add(t)
      })
      this
    }

    /**
     * add Double signals to export
     */
    def addDouble(signals: Seq[Double]*) = {
      signals.toArray.foreach(signal => {
        list.add(manager.create(signal.toArray))
      })
      this
    }

    /**
     * add named Double signals to export
     * @param signals map of name to signal
     */
    def addDouble(signals: Map[String, Seq[Double]]) = {
      signals.toArray.foreach(signal => {
        val t = manager.create(signal._2.toArray)
        t.setName(signal._1)
        list.add(t)
      })
      this
    }

    /**
     * add named 2d BigDecimal signals to export
     *
     * caution: this function convert BigDecimal to Double,
     * might cost too much time when signal is long
     *
     * @param signals map of name to signal
     */
    def add2d(signals: Map[String, Seq[Signal]]) = {
      signals.toArray.foreach(signal => {
        val t = manager.create(signal._2.toArray.map(_.toArray.map(_.toDouble)))
        t.setName(signal._1)
        list.add(t)
      })
      this
    }

    /**
     * add named 2d Double signals to export
     * @param signals map of name to signal
     */
    def addDouble2d(signals: Map[String, Seq[Seq[Double]]]) = {
      signals.toArray.foreach(signal => {
        val t = manager.create(signal._2.toArray.map(_.toArray))
        t.setName(signal._1)
        list.add(t)
      })
      this
    }

    /**
     * save signals to target file
     */
    def savez() = {
      val os      = Files.newOutputStream(file.toPath)
      list.encode(os, true)
      file
    }
  }

  /** export one BigDecimal signal
   *  caution: this function convert BigDecimal to Double,
   *  might cost too much time when signal is long
   */
  def exportSignal(file: File, signal: Signal): File = pythonExporter(file).add(signal).savez()

  /** export one Double signal
   */
  def exportSignalDouble(file: File, signal: Seq[Double]): File = pythonExporter(file).addDouble(signal).savez()

  /**
   * export signals to a npz file
   * @param file target file
   */
  case class pythonImporter(file: File = outputArrayFile) {

    private val allocatedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    private val freeMemory = Runtime.getRuntime.maxMemory() - allocatedMemory
    private val neededMemory = getSignalSizeDouble(file)

    // check memory requirement
    assert(freeMemory * 0.9 > neededMemory, s"Warning: not enough memory to import signal, needed: $neededMemory, available: $freeMemory")

    private val manager = NDManager.newBaseManager()

    private val is          = Files.newInputStream(file.toPath)
    private val decoded     = NDList.decode(manager, is)
    private val signalCount = decoded.size()

    (0 until signalCount).map(decoded.get).map(_.getShape).map(_.dimension())

    /**
     * import BigDecimal signal
     *
     * caution: this function convert Double to BigDecimal,
     * might cost too much time when signal is long
     */
    def importBigDecimal: Seq[Signal] = {
      (0 until signalCount)
        .map(decoded.get)
        .map(_.toDoubleArray.toSeq.map(BigDecimal.valueOf))
    }

    /**
     * import named BigDecimal signal
     *
     * caution: this function convert Double to BigDecimal,
     * might cost too much time when signal is long
     *
     * @return map of name to signal
     */
    def importNamedBigDecimal: Map[String, Signal] = {
      (0 until signalCount)
        .map(decoded.get)
        .map(arr => {
          arr.getName.replace(".npy", "") ->
            arr.toDoubleArray.toSeq.map(BigDecimal.valueOf)
        })
        .toMap
    }

    /**
     * import Double signal
     */
    def importDouble: Seq[Seq[Double]] = {
      (0 until signalCount)
        .map(decoded.get)
        .map(_.toDoubleArray.toSeq)
    }

    /**
     * import Double signal
     * @return map of name to signal
     */
    def importNamedDouble: Map[String, Seq[Double]] = {
      (0 until signalCount)
        .map(decoded.get)
        .map(arr => arr.getName.replace(".npy", "") -> arr.toDoubleArray.toSeq)
        .toMap
    }

    /**
     * import named 2d BigDecimal signal
     *
     * caution: this function convert Double to BigDecimal,
     * might cost too much time when signal is long
     *
     * @return map of name to signal
     */
    def importNamedBigDecimal2d: Map[String, Seq[Signal]] = {
      (0 until signalCount)
        .map(decoded.get)
        .filter(_.getShape.dimension() == 2)
        .map(arr => {
          val arrDouble = arr.toDoubleArray
          val name = arr.getName.replace(".npy", "")
          val shape = arr.getShape.getShape
          val data: ArrayBuffer[Signal] = ArrayBuffer()
          (0 until shape.head.toInt).foreach(i => {
            data.append(arrDouble.map(BigDecimal.valueOf).slice(i * shape(1).toInt, i * shape(1).toInt + shape(1).toInt))
          })
          name -> data
        }).toMap
    }

    /**
     * import named 2d Double signal
     *
     * caution: this function convert Double to BigDecimal,
     * might cost too much time when signal is long
     *
     * @return map of name to signal
     */
    def importNamedDouble2d: Map[String, Seq[Seq[Double]]] = {
      (0 until signalCount)
        .map(decoded.get)
        .filter(_.getShape.dimension() == 2)
        .map(arr => {
          val arrDouble = arr.toDoubleArray
          val name = arr.getName.replace(".npy", "")
          val shape = arr.getShape.getShape
          val data: ArrayBuffer[Seq[Double]] = ArrayBuffer()
          (0 until shape.head.toInt).foreach(i => {
            data.append(arrDouble.slice(i * shape(1).toInt, i * shape(1).toInt + shape(1).toInt))
          })
          name -> data
        }).toMap
    }
  }

  /** import one BigDecimal signal
   *  caution: this function convert Double to BigDecimal,
   *  might cost too much time when signal is long
   */
  def importSignal(file: File): Signal = pythonImporter(file).importBigDecimal.head

  /** import one Double signal
   *  caution: this function convert Double to BigDecimal,
   *  might cost too much time when signal is long
   */
  def importSignalDouble(file: File): Seq[Double] = pythonImporter(file).importDouble.head

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
