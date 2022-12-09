package Chainsaw.dsp

import Chainsaw.ChainsawMetric.{doubleBound, forallBound}
import Chainsaw._
import spinal.core._
import spinal.lib.CounterFreeRun

import scala.math.Pi
import scala.language.postfixOps

case class PointwiseUnwrap(dataType: NumericType)
  extends ChainsawGenerator {

  override def name = getAutoName(this)

  override def impl(dataIn: Seq[Any]): Seq[Double] = {
    val unwrapped = matlabEngine.feval("unwrap", dataIn.asInstanceOf[Seq[Double]].toArray).asInstanceOf[Array[Double]]
    Seq(unwrapped.last)
  }

  override def generateTestCases = Seq.fill(1000)(SFixInfo(dataType.integral - 1, dataType.fractional).getRandom)

  override val metric = ChainsawMetric(frameWise = forallBound(doubleBound(1e-1)))

  override def inputTypes = Seq.fill(2)(dataType: NumericType)

  override def outputTypes = Seq(dataType)

  override def inputFormat = inputNoControl

  override def outputFormat = outputNoControl

  override def latency = 5

  /** -------- implementations
   * --------
   */
  override def implH: ChainsawModule = new ChainsawModule(this) {
    // stage 0
    val Seq(a, b) = sfixDataIn // TODO: full-width prev data is not necessary

    val piReciprocal = dataType.fromConstant(1.0 / Pi)
    val pi = dataType.fromConstant(Pi)
    // 0 -> 1
    val prev = (a * piReciprocal).d()
    val next = (b * piReciprocal).d()
    // unwrap for normalized phase value
    val (m, l0) = prev.asBits.splitAt(-prev.minExp)
    val (n, l1) = next.asBits.splitAt(-next.minExp)

    // 1 -> 2
    val Seq(mPlus, mMinus) = Seq(m.asSInt + 1, m.asSInt - 1).map(_.d())
    val det0 = (l1.asUInt > l0.asUInt).d()

    //    val random = CounterFreeRun(13) // avoid accumulation of bias
    // 2 -> 3
    val mux0 = Mux(det0, mMinus, mPlus).d()

    // 3 -> 4
    val det1 = (m.lsb === n.lsb).d(2)
    val mux1 = Mux(det1, m.asSInt.d(2), mux0).d()

    val ret = cloneOf(prev)
    ret.assignFromBits(mux1 ## l1.d(3))

    // 4 -> 5
    sfixDataOut.head := (ret * pi).d().truncated
  }

}

