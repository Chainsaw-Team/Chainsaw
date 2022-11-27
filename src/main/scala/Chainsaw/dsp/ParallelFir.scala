package Chainsaw.dsp

import Chainsaw.ChainsawMetric.{doubleBound, forallBound}
import Chainsaw.NumericExt._
import Chainsaw._
import Chainsaw.xilinx.VivadoUtilRequirement
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

case class ParallelFir(coeffs: Seq[Double], filterPrecision: FilterPrecision, parallel: Int)
  extends ChainsawGenerator {

  val upSample = 1

  override def name = getAutoName(this)

  import filterPrecision._

  val taps = coeffs.length
  val coeffMaxExp = log2Up(coeffs.map(_.abs.ceil.toInt).max)
  require(coeffMaxExp <= coeffType.integral, "your coeffs are too large for the given coeffType")

  /** --------
   * taps calculation
   * -------- */


  val phaseCount = parallel * upSample
  val coeffsPadded = coeffs.padTo(taps.nextMultipleOf(phaseCount), 0.0)
  val subFilterTaps = coeffsPadded.length / phaseCount
  // poly phase decomposition of coefficients
  val coeffGroups = (0 until phaseCount).map(i => coeffsPadded.zipWithIndex.filter(_._2 % phaseCount == i).map(_._1))
  val subFilterGens = coeffGroups.map(coeff => Fir(coeff, filterPrecision))
  val subFilterLatency = subFilterGens.head.latency

  /** --------
   * poly phase network construction
   * -------- */
  // TODO: apply strength reduction for polyphase filter

  def decomposition(symbol: String, order: Int) = {
    val terms = (0 until order).map(i => TermZDomain(i, Seq((symbol, i))))
    PolyZDomain(terms)
  }

  val termsX = (0 until parallel).map(i => TermZDomain(-i * upSample, Seq(("x", i * upSample))))
  val termsH = (0 until phaseCount).map(i => TermZDomain(-i, Seq(("h", i))))

  def getPolyPhase(p: PolyZDomain, n: Int) = {
    def getPhase(i: Int) = p.terms.filter(_.factors.map(_._2).sum % n == i)

    (0 until n).map(i => getPhase(i).map(_.z(i)))
  }

  val termYs: Seq[Seq[TermZDomain]] = {
    val product = PolyZDomain(termsX) * PolyZDomain(termsH)
    getPolyPhase(product, parallel * upSample)
  }

  logger.info(
    s"\n----polyphase filter report----" +
      s"\n\t${termYs.zipWithIndex.map { case (terms, i) => s"Y$i = ${terms.mkString(" + ")}" }.mkString("\n\t")}")

  // latency of the adderTree
  val sumLatency = log2Up(termYs.head.length)
  val lengthDrop = (subFilterTaps - 1) * phaseCount

  override def impl(dataIn: Seq[Any]) =
    fir(dataIn.asInstanceOf[Seq[Double]], coeffsPadded).drop(coeffsPadded.length - 1)

  override val implMode = Infinite

  //  override val metric = ChainsawMetric(frameWise = forallBound(doubleBound(1e-2)))
  override val metric = ChainsawMetric(frameWise = corrMetric) // magnitude-irrelevant metric

  override def generateTestCases: Seq[Double] = Seq.fill(1000)(Random.nextDouble())

  override def inputTypes = Seq.fill(parallel)(dataType)

  override def outputTypes = Seq.fill(parallel * upSample)(retType)

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl

  override def latency = subFilterLatency + sumLatency

  logger.info(s"sumLatency = $sumLatency, subFilterLatency = $subFilterLatency")

  override def offset = parallel - 1

  override def utilEstimation = VivadoUtilRequirement(dsp = coeffsPadded.length * parallel)

  logger.info(s"dsp estimation = ${utilEstimation.dsp} for $name")

  //  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawModule(this) {

    val rets = termYs.map { terms =>
      val subFilterRets = terms.map { term =>
        val inPort = sfixDataIn(term.getIndexOf("x") / upSample)
        val subFilter = subFilterGens(term.getIndexOf("h"))
        val ret = subFilter.asFunc(Seq(inPort.asBits)).head.toSFix(retType)
        val delay = -(term.z / parallel) // divided by sizeOut as the system is running at a higher speed
        ret.d(delay)
      }

      def add(a: SFix, b: SFix) = {
        val ret = a + b
        ret.addAttribute("use_dsp", "no") // this is a must
      }

      def pipeline(value: SFix, nothing: Int) = value.d()

      subFilterRets.reduceBalancedTree(add, pipeline)
    }

    sfixDataOut := rets
  }
}
