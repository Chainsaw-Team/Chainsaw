package Chainsaw

import breeze.math.Complex
import breeze.numerics.abs
import Chainsaw.ChainsawMetric.defaultBound

case class ChainsawMetric(
                           elementWise: Metric = defaultBound,
                           frameWise: FrameMetric
                         )

object ChainsawMetric { // common metrics

  /** --------
   * elementwise metrics
   -------- */

  def defaultBound: Metric = (y: Any, g: Any) => y == g

  def doubleBound(epsilon: Double): Metric = (y: Any, g: Any) => abs(y.asInstanceOf[Double] - g.asInstanceOf[Double]) < epsilon

  def complexBound(epsilon: Double): Metric = (y: Any, g: Any) => abs(y.asInstanceOf[Complex] - g.asInstanceOf[Complex]) < epsilon

  def berBound(ber: Double, elementWise: (Any, Any) => Boolean): FrameMetric = (ys: Seq[Any], gs: Seq[Any]) => {
    val errorCount = ys.zip(gs).count { case (y, g) => !elementWise(y, g) }
    val ret = errorCount.toDouble / ys.length <= ber
    logger.info(s"frame ber: ${errorCount.toDouble / ys.length}")
    ret
  }

  /** --------
   * frame-wise metrics
   -------- */
  def forallBound(elementWise: (Any, Any) => Boolean): FrameMetric = (ys: Seq[Any], gs: Seq[Any]) => {
    ys.zip(gs).forall { case (y, g) => elementWise(y, g) }
  }

  def complexAbs(epsilon: Double) = ChainsawMetric(complexBound(epsilon), berBound(0, complexBound(epsilon)))

  def doubleAbs(epsilon: Double) = ChainsawMetric(doubleBound(epsilon), berBound(0, doubleBound(epsilon)))

  /** default metric which require all elements in yours and golden are exactly the same
   */
  def defaultMetric = ChainsawMetric(defaultBound, forallBound(defaultBound))
}