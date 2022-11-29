package Chainsaw

import java.io.File
import scala.language.implicitConversions
import com.mathworks.matlab.types.Struct

package object dsp {

  type Signal = Array[Double]

  def fir(data: Seq[Double], coeffs: Seq[Double]) =
    matlabEngine.feval("filter", coeffs.toArray, Array(1.0), data.toArray).asInstanceOf[Array[Double]]

  def upSample(dataIn: Signal, up: Int): Signal = dataIn.flatMap(d => d +: Seq.fill(up - 1)(0.0))

  def conv(a: Signal, b: Signal): Signal = {
    val padded = Seq.fill(b.length - 1)(0.0) ++ a ++ Seq.fill(b.length - 1)(0.0)
    padded.sliding(b.length).map(_.zip(b).map { case (a, b) => a * b }.sum).toArray
  }

  def downSample(dataIn: Signal, down: Int): Signal = dataIn.grouped(down).map(_.head).toArray

  def getCorr(yours: Signal, golden: Signal) = {
    val length = yours.length min golden.length
    val a = yours.take(length)
    val b = golden.take(length)
    matlabEngine.eval(s"addpath('${matlabScriptDir.getAbsolutePath}')")
    val ret = matlabEngine.feval("getCorr", a, b).asInstanceOf[Double]
    logger.info(s"corr factor = $ret")
    ret
  }

  val corrMetric = (yours: Seq[Any], golden: Seq[Any]) => {
    val y = yours.asInstanceOf[Seq[Double]].toArray
    val g = golden.asInstanceOf[Seq[Double]].toArray
    getCorr(y, g) > 0.9
  }

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
}
