package Chainsaw.dsp

import Chainsaw._
import spinal.core.{RegNextWhen, _}
import spinal.lib._
import NumericExt._
import ChainsawMetric._

/** discrete filter, which behaves exactly the same as Matlab ''filter'' function
 *
 * @param bs        numerator of the rational transfer function
 * @param as        denominator of the rational transfer function
 * @param coeffType precision of the coefficient
 * @param dataType  precision of the input/output data
 * @see matlab function: filter
 */
case class Filter(bs: Seq[Double], as: Seq[Double], coeffType: NumericType, dataType: NumericType)
  extends ChainsawGenerator {

  override def name = s"fir_b_${bs.map(_.toInt).mkString("_")}_a_${as.map(_.toInt).mkString("_")}".replace("-", "N")

  /** --------
   * reference model
   * -------- */
  override def impl(dataIn: Seq[Any]) = {
    val b = bs.toArray
    val a = as.toArray
    matlabEngine.feval("filter", b, a, dataIn.asInstanceOf[Seq[Double]].toArray)
      .asInstanceOf[Array[Double]]
  }

  override val metric = ChainsawMetric(frameWise = forallBound(doubleBound(1e-1)))

  override val implMode = Infinite

  /** --------
   * size & timing
   * -------- */
  override def inputTypes = Seq(dataType)
  override def outputTypes = Seq(dataType)

  override def inputFormat = inputNoControl
 override def outputFormat = outputNoControl

  override def latency = 2

  override def implH = new ChainsawModule(this) {

    def zero: SFix = dataType.fromConstant(0.0)

    // transposed direct-2
    val x = RegNextWhen(sfixDataIn.head, init = zero, cond = validIn)
    val ret = dataType.asSFix()
    val forwards = bs.map(b => x * coeffType.fromConstant(b)).reverse
    val backwards = as.tail.map(a => ret * coeffType.fromConstant(-a)).reverse
    val sums = forwards.zip(backwards).map { case (a, b) => a + b }
    sums.zipWithIndex.foreach { case (fix, i) => fix.setName(s"sum_$i") }
    ret := (sums :+ forwards.last).reduce { (prev, next) =>
      Delay(prev, 1, init = zero
      ) + next
    }.truncate(dataType.asSFix)

    sfixDataOut.head := ret.d()
  }
}
