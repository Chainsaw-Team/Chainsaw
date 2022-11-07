package Chainsaw

import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write

import java.io._

/** waveform of a signal
 *
 * @param name signal name
 * @param wave signal lane activity
 * @param data signal labels
 * @see [[https://github.com/wavedrom/schema/blob/master/WaveJSON.md]] for full syntax of "wave" attribute
 */
case class Waveform(name: String, wave: String, data: Seq[String]) {

  def toGlitchFree(wave: String) =
    wave.head +: (1 until wave.length).map { i =>
      val char = wave(i)
      val charPre = wave(i - 1)
      if ((char == '0' || char == '1') && char == charPre) '.' else char
    }.mkString("")

  def toJson = Map("name" -> name, "wave" -> toGlitchFree(wave), "data" -> data)
}

/** a waveform graph which contains multiple waveforms
 *
 * @param name  json file name
 * @param waves waveforms contained
 */
case class WaveformGraph(name: String, waves: Seq[Waveform]) {

  def toJson: (String, Seq[Equals]) = {
    val period = waves.map(_.wave.length).max
    // add clk at the first cycle as a reference
    "signal" -> (Waveform("clk", "p" + "." * (period - 1), Seq()) +: waves.map(_.toJson))
  }

  val targetDir = new File("./src/main/resources/waveformGenerated")
  val jsonFile = new File(targetDir, s"$name.json")

  def generateJsonFile(): Unit = {

    implicit val format: AnyRef with Formats = Serialization.formats(NoTypeHints)
    val json: String = write(toJson) // get content
    targetDir.mkdirs()
    jsonFile.createNewFile()
    val pw = new PrintWriter(jsonFile)
    pw.write(json)
    pw.close()

    println(s"your waveform file generated:\n ${jsonFile.getAbsolutePath}")
  }
}

object ShowWaveformGraph extends App {
  val wave0 = Waveform("w0", "====", (0 until 4).map("x" + _.toString))
  val wave1 = Waveform("w1", "====", (5 until 8).map("x" + _.toString))
  val fig0 = WaveformGraph("example", Seq(wave0, wave1))
  val fig1 = WaveformGraph("another", Seq(wave0, wave1))
  fig0.generateJsonFile()
  fig1.generateJsonFile()
}
