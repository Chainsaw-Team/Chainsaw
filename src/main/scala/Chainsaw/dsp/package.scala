package Chainsaw

import spinal.core.{Data, _}

import java.io.File
import scala.language.implicitConversions

package object dsp {

  type MatlabSignal = Array[Double]   // datatype for Matlab interaction
  type Signal       = Seq[BigDecimal] // datatype for golden model

  /** -------- frequently used functions
    * --------
    */

  def fir(data: Seq[Double], coeffs: Seq[Double]) =
    matlabEngine
      .feval("filter", coeffs.toArray, Array(1.0), data.toArray)
      .asInstanceOf[Array[Double]]

  def conv(a: MatlabSignal, b: MatlabSignal): MatlabSignal = { // linear convolution
    val padded = Seq.fill(b.length - 1)(0.0) ++ a ++ Seq.fill(b.length - 1)(0.0)
    padded
      .sliding(b.length)
      .map(_.zip(b).map { case (a, b) => a * b }.sum)
      .toArray
  }

  def downSample(dataIn: Signal, down: Int): Signal =
    dataIn.grouped(down).map(_.head).toSeq

  def upSample(dataIn: Signal, up: Int): Signal =
    dataIn.flatMap(d => d +: Seq.fill(up - 1)(BigDecimal(0.0)))

  /** read simulink output in time series format
    *
    * @param file
    *   filepath
    */
  def getTimeSeries[T](file: File): T = {
    logger.info(s"loading ${file.getAbsolutePath} ...")
    matlabEngine.eval(s"ret = load('${file.getAbsolutePath}').ans.Data;")
    matlabEngine.eval(s"ret = double(ret);")
    matlabEngine.getVariable("ret").asInstanceOf[T]
  }

  /** -------- modeling Z domain
    * --------
    */
  implicit def termAsPoly(term: TermZDomain): PolyZDomain = PolyZDomain(
    Seq(term)
  )

  // model of z-domain arithmetic
  case class TermZDomain(z: Int, factors: Seq[(String, Int)]) {

    def getIndicesOf(symbol: String) = factors.filter(_._1 == symbol).map(_._2)

    def getIndexOf(symbol: String) = getIndicesOf(symbol).head

    def *(that: TermZDomain) = {
      TermZDomain(z + that.z, factors ++ that.factors)
    }

    def +(that: TermZDomain) = PolyZDomain(Seq(this, that))

    def z(cycle: Int): TermZDomain = TermZDomain(z + cycle, factors)

    override def toString =
      factors.map { case (sym, i) => s"$sym$i" }.mkString("") + s"z$z"
  }

  case class PolyZDomain(terms: Seq[TermZDomain]) {

    override def toString = terms.mkString(" + ")

    def +(that: PolyZDomain) = PolyZDomain(terms ++ that.terms)

    def *(that: PolyZDomain) = PolyZDomain(
      for (t1 <- terms; t2 <- that.terms) yield t1 * t2
    )
  }

  /** -------- Hardware Utils
    * --------
    */
  def dataWithControl(
      dataType: NumericType,
      parallel: Int,
      controlWidth: Int
  ) = {
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

  // python utils
  import Chainsaw.io.pythonIo._
  def plotSpectrum(signal: Signal, samplingFreq: HertzNumber) = {
    exportSignal(inputArrayFile, signal)
    val pyPath = new File("goldenModel/utils/plot_spectrum.py")
    runPython(pyPath, samplingFreq.toDouble.toString)
  }

  def corrMetric(yours: Signal, golden: Signal, threshold: Double) = {
    pythonExporter(inputArrayFile).add(yours, golden)
    val pyPath   = new File(pythonProjectDir ,"corr_metric.py")
    val rets     = runPython(pyPath).split(" ")
    val corrcoef = rets(0).toDouble
    val lag      = rets(1).toInt
    corrcoef >= threshold
  }

  def designFilter(
      tap: Int,
      target: Seq[HertzNumber],
      samplingFreq: HertzNumber,
      filterType: String
  ): Signal = {
    val pyPath = new File(pythonProjectDir, "design_filter.py")
    runPython(
      pyPath,
      s"$tap [${target.map(_.toDouble).mkString(", ")}] ${samplingFreq.toDouble} $filterType"
    )
    importSignal(outputArrayFile)
  }

}
