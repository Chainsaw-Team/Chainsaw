package Chainsaw.dsp

import Chainsaw.ChainsawMetric.{doubleBound, forallBound}
import Chainsaw._
import spinal.core._
import spinal.lib.CounterFreeRun

import scala.math.Pi
import scala.language.postfixOps

case class PointWiseUnwrap(dataType: NumericType) extends ChainsawGenerator {
  override def name = "unwrap"

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

  override def latency = 2

  /** -------- implementations
   * --------
   */
  override def implH: ChainsawModule = new ChainsawModule(this) {
    // stage 0
    val Seq(a, b) = sfixDataIn // TODO: full-width prev data is not necessary

    val piReciprocal = dataType.fromConstant(1.0 / Pi)
    val pi = dataType.fromConstant(Pi)
    val prev = a * piReciprocal
    val next = b * piReciprocal

    // unwrap for normalized phase value
    val (m, l0) = prev.asBits.splitAt(-prev.minExp)
    val (n, l1) = next.asBits.splitAt(-next.minExp)

    val random = CounterFreeRun(13) // avoid accumulation of bias
    // 0 -> 1
    val mux0 = SInt(m.getBitsWidth bits)
    when(l1.asUInt > l0.asUInt)(mux0 := m.asSInt - 1)
      .elsewhen(l1.asUInt < l0.asUInt)(mux0 := m.asSInt + 1)
      .elsewhen(l1.asUInt === l0.asUInt && random.value.lsb)(mux0 := m.asSInt - 1)
      .otherwise(mux0 := m.asSInt + 1)
    // 1 -> 2

    val mux1 = Mux((m.lsb === n.lsb).d(), m.asSInt.d(), mux0.d()).d()
    val ret = cloneOf(prev)
    ret.assignFromBits(mux1 ## l1.d(2))

    sfixDataOut.head := (ret * pi).truncated
  }

  override def implNaiveH = Some(implH)
}

