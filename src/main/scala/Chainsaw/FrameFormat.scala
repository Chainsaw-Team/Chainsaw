package Chainsaw

import scala.collection.mutable.ArrayBuffer

/**
 * @param flow a 2-dimensional matrix in which each row is a cycle, a each column is a port,
 *             element >= 0 means valid, elements share a same value should be the same
 */
case class FrameFormat(flow: Seq[Seq[Int]]) {

  /** --------
   * properties
   * -------- */
  def period: Int = flow.length

  def portSize: Int = flow.head.length

  def rawData: Seq[Int] = flow.flatten.filter(_ >= 0)

  def rawDataCount: Int = rawData.distinct.length

  def isUnique = rawData.length == rawData.distinct.length

  def isCompact = !flow.flatten.contains(-1)

  /** --------
   * query
   * -------- */
  def getTime(elem: Int) = flow.flatten.indexWhere(_ == elem) / portSize

  def getPort(elem: Int) = flow.flatten.indexWhere(_ == elem) % portSize


  /** --------
   * methods for readability & visualization
   * -------- */
  // frame format in dot notation
  override def toString = {
    val charChange = (index: Int) => if (index < 0) " " else dot
    s"data flow: portSize=$portSize, period=$period, size=${portSize * period}, raw=$rawDataCount\n$dot for valid:\n${
      flow.zipWithIndex.map { case (seq, i) =>
        s"cycle$i ".padTo(10, ' ') + "|" + seq.map(charChange(_)).mkString("") + "|"
      }.mkString("\n")
    }"
  }

  /** generate the waveform figure as json file, which can be rendered by VS Code plugin "Waveform Render"
   *
   * @param name   name of the json file
   * @param symbol symbol used for elements in waveform file, x -> x_0, x_1...
   * @see [[Waveform]]
   */
  def generateWaveform(name: String, symbol: String = "x"): Unit = {

    def toWave(index: Int) = if (index < 0) "x" else "="

    def toData(index: Int) = symbol + index.toString

    def toDataPrime(index: Int) = symbol + "\'" + index.toString

    def addPrePost(wave: String) = "d" + wave + "d"

    val waves: Seq[String] = flow.transpose.map(seq => seq.map(toWave).mkString(""))
    val data: Seq[Seq[String]] = flow.transpose.map(seq => seq.filter(_ > -1).map(toData) ++ seq.filter(_ > -1).map(toDataPrime))

    val waveforms = waves.zip(data).zipWithIndex.map { case ((wave, data), i) => Waveform(s"port$i", addPrePost(wave.repeat(2)), data) }
    val valid = Waveform("valid", addPrePost(flow.map(seq => "1").mkString("").repeat(2)), Seq())
    val last = Waveform("last", addPrePost(("0" * (period - 1) + "1").repeat(2)), Seq())

    WaveformGraph(name, waveforms :+ last :+ valid).generateJsonFile()
  }

  /** --------
   * simulation utils
   * -------- */
  // build input frame from raw data
  def fromRawData[T](seq: Seq[T], zero: T) = {
    val data = flow.map(_.map(index => if (index >= 0) seq(index) else zero)) //
    val valid = flow.map(_ => true)
    val last = Seq.fill(period - 1)(false) :+ true
    (data, valid, last)
  }

  // extract raw data from output frame
  def toRawData[T](frame: Seq[Seq[T]]) = {
    require(frame.length == period && frame.head.length == portSize,
      s"period should be $period while it is ${frame.length}, portWidth should be $portSize while it is ${frame.head.length}")
    val rawDataTemp = Seq.fill(rawDataCount)(ArrayBuffer[T]())
    flow.zip(frame).foreach { case (flowRow, dataRow) =>
      flowRow.zip(dataRow).foreach { case (index, data) =>
        if (index >= 0) rawDataTemp(index) += data
      }
    }
    require(rawDataTemp.forall(valuesForSameIndex => valuesForSameIndex.distinct.length == valuesForSameIndex.length))
    rawDataTemp.map(_.head)
  }

  /** --------
   * format algebra
   * -------- */
  def getBubble: Seq[Int] = flow.head.map(_ => -1)

  // add bubble cycles after current frame
  def pad(cycle: Int) = {
    val padded = flow ++ Seq.fill(cycle)(getBubble)
    FrameFormat(padded)
  }

  // add bubble cycles between current cycles
  def interpolate(multiple: Int): FrameFormat = {
    val interpolated = flow.flatMap(row => row +: Seq.fill(multiple - 1)(getBubble))
    FrameFormat(interpolated)
  }

  // repeat current frame
  def repeat(multiple: Int): FrameFormat = {
    def next(current: Seq[Int]): Seq[Int] = current.map(i => if (i < 0) -1 else i + rawDataCount)

    val repeated = Seq.iterate(flow.flatten, multiple)(next).flatten
    FrameFormat(repeated.grouped(portSize).toSeq)
  }
}

object FrameFormat {
  def apply(flow: Seq[Int], portSize: Int): FrameFormat = {
    require(flow.length % portSize == 0)
    FrameFormat(flow.grouped(portSize).toSeq)
  }

}

object MatrixFormat {
  def apply(streamWidth: Int, period: Int) = {
    val flow = (0 until streamWidth * period).grouped(streamWidth).toSeq
    FrameFormat(flow)
  }
}

// user case
object ShowFrameFormat extends App {

  // build a frame manually
  val basic = FrameFormat(Seq(-1, 0, 1, 2, 3, 4, 5, -1), 4)
  println(s"basic = $basic")
  // build a frame of certain pattern(by factory objects)
  val matrix = MatrixFormat(3, 5)
  println(matrix)

  // frame arithmetics
  println(s"basic.repeat(2) = ${basic.repeat(2)}")
  println(s"basic.pad(2) = ${basic.pad(2)}")
  println(s"basic.interpolate(2) = ${basic.interpolate(2)}")
  println(s"matrix.interpolate(3) = ${matrix.interpolate(3)}")

  // data format conversion
  val rawData = Seq('a', 'b', 'c', 'd', 'e', 'f')
  println(s"rawData = ${rawData.mkString(" ")}")
  println("raw -> frame")
  val frame = basic.fromRawData(rawData, 'x')._1
  println(frame.map(_.mkString(" ")).mkString("\n"))
  println("frame -> raw")
  println(basic.toRawData(frame).mkString(" "))

}