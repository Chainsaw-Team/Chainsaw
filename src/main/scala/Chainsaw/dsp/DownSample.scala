//package Chainsaw.dsp
//
//import Chainsaw._
//import Chainsaw.memory._
//import Chainsaw.xilinx._
//import spinal.core._
//import spinal.lib._
//
//case class DownSample(factor: Int, dynamic: Boolean, dataType: NumericTypeNew, parallel: Int)
//  extends ChainsawDspGenerator {
//  override def name = getAutoName(this)
//
//  override def model = new ChainsawInfiniteModel {
//    override def offset = 0
//
//    override def impl(data: Seq[BigDecimal]) = downSample(data, factor)
//
//    override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
//      correlationMetric(yours, golden, 0.9)
//
//    override def testCases =
//      if (!dynamic) Seq.fill(1000)(dataType.random)
//      else Seq.fill(1000)(Seq.fill(parallel)(dataType.random) :+ BigDecimal(factor / 3)).flatten
//
//    override def latency = 0
//  }
//
//  override def inputTypes =
//    if (!dynamic) dataWithControl(dataType, parallel, log2Up(factor + 1))
//    else Seq.fill(parallel)(dataType)
//
//  override def outputTypes = Seq.fill(parallel)(dataType)
//
//  override def vivadoUtilEstimation = VivadoUtilEstimation()
//
//  override def fmaxEstimation = 600e6
//
//  override def implH = new ChainsawDspModule(this) {
//
//    val payload = dataIn.take(parallel)
//    lazy val control = dataIn.last.asUInt()
//
//    if (!dynamic) { // TODO: for parallel > 1, using lcm
//      require(parallel == 1)
//      val counter = CounterFreeRun(factor) // TODO: should be driven by valid
//      validOut := counter.willOverflow
//      lastOut := False
//      dataOut := payload
//    } else {
//      val counter = DynamicCounter(factor)
//      counter.increment()
//      validOut := counter.willOverflow
//      lastOut := False
//      dataOut := payload
//    }
//  }
//}
