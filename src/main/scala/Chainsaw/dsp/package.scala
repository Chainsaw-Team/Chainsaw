package Chainsaw

import java.io.File
import scala.language.implicitConversions
import com.mathworks.matlab.types.Struct
import spinal.core.Data
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

package object dsp {

  type MatlabSignal = Array[Double] // datatype for Matlab interaction
  type Signal = Seq[BigDecimal] // datatype for golden model

  /** --------
   * frequently used functions
   * -------- */

  def fir(data: Seq[Double], coeffs: Seq[Double]) =
    matlabEngine.feval("filter", coeffs.toArray, Array(1.0), data.toArray).asInstanceOf[Array[Double]]

  def conv(a: MatlabSignal, b: MatlabSignal): MatlabSignal = { // linear convolution
    val padded = Seq.fill(b.length - 1)(0.0) ++ a ++ Seq.fill(b.length - 1)(0.0)
    padded.sliding(b.length).map(_.zip(b).map { case (a, b) => a * b }.sum).toArray
  }

  def downSample(dataIn: Signal, down: Int): Signal = dataIn.grouped(down).map(_.head).toSeq

  def upSample(dataIn: Signal, up: Int): Signal = dataIn.flatMap(d => d +: Seq.fill(up - 1)(BigDecimal(0.0)))


  /** read simulink output in time series format
   *
   * @param file filepath
   */
  def getTimeSeries[T](file: File): T = {
    logger.info(s"loading ${file.getAbsolutePath} ...")
    matlabEngine.eval(s"ret = load('${file.getAbsolutePath}').ans.Data;")
    matlabEngine.eval(s"ret = double(ret);")
    matlabEngine.getVariable("ret").asInstanceOf[T]
  }

  // TODO: implement xcorr in Scala, and paint the result by matplotlib, rather than Matlab
  def getCorr(yours: MatlabSignal, golden: MatlabSignal) = {
    val length = yours.length min golden.length
    val a = yours.take(length)
    val b = golden.take(length)
    matlabEngine.eval(s"addpath('${matlabScriptDir.getAbsolutePath}')")
    val ret = matlabEngine.feval("getCorr", a, b).asInstanceOf[Double]
    logger.info(s"corr factor = $ret")
    if (ret < 0.9) {
      val pngFile = new File("src/main/resources/matlabGenerated/corr.png")
      logger.info(s"view ${pngFile.getAbsolutePath}")
    }
    ret
  }

  // TODO: implement this without Matlab
  val corrMetric = (yours: Seq[Any], golden: Seq[Any]) => {
    val y = yours.asInstanceOf[Seq[Double]].toArray
    val g = golden.asInstanceOf[Seq[Double]].toArray
    getCorr(y, g) > 0.9
  }

  def plot(signal: MatlabSignal, name: String): Unit = {
    matlabEngine.putVariable("data", signal)
    matlabEngine.eval(s"plot(data); title('$name');")
    val pngFile = new File(s"src/main/resources/matlabGenerated/$name.png")
    matlabEngine.eval(s"saveas(gcf,'${pngFile.getAbsolutePath}')")
  }

  /** --------
   * modeling Z domain
   * -------- */
  implicit def termAsPoly(term: TermZDomain): PolyZDomain = PolyZDomain(Seq(term))

  // model of z-domain arithmetic
  case class TermZDomain(z: Int, factors: Seq[(String, Int)]) {

    def getIndicesOf(symbol: String) = factors.filter(_._1 == symbol).map(_._2)

    def getIndexOf(symbol: String) = getIndicesOf(symbol).head

    def *(that: TermZDomain) = {
      TermZDomain(z + that.z, factors ++ that.factors)
    }

    def +(that: TermZDomain) = PolyZDomain(Seq(this, that))

    def z(cycle: Int): TermZDomain = TermZDomain(z + cycle, factors)

    override def toString = factors.map { case (sym, i) => s"$sym$i" }.mkString("") + s"z$z"
  }

  case class PolyZDomain(terms: Seq[TermZDomain]) {

    override def toString = terms.mkString(" + ")

    def +(that: PolyZDomain) = PolyZDomain(terms ++ that.terms)

    def *(that: PolyZDomain) = PolyZDomain(for (t1 <- terms; t2 <- that.terms) yield t1 * t2)
  }

  /** --------
   * Hardware Utils
   * -------- */
  def dataWithControl(dataType: NumericType, parallel: Int, controlWidth: Int) = {
    Seq.fill(parallel)(dataType) :+ NumericType.U(controlWidth)
  }

  def splitDataWithControl(raw: Seq[BigDecimal], parallel: Int) = {
    val payload = raw.grouped(parallel + 1).flatMap(_.init).toSeq
    val control = raw.grouped(parallel + 1).map(_.last).toSeq
    (payload, control)
  }

  def setAsOutput[T <: Data](data: T) = {
    val ret = out cloneOf data
    ret := data
    ret
  }

}
