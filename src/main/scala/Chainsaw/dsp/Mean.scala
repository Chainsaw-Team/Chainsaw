//package Chainsaw.dsp
//
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.lib.fsm._
//
//import Chainsaw._
//import Chainsaw.xilinx._
//
//case class Mean(size: Int, dynamic: Boolean, dataType: NumericTypeNew, parallel: Int)
//  extends ChainsawDspGenerator {
//
//  require(size % parallel == 0)
//  val retType = Seq.fill(size)(dataType).reduce(_ + _)
//
//  override def name = getAutoName(this)
//
//  override def model = new ChainsawInfiniteModel {
//    override def offset = 0
//
//    override def impl(data: Seq[BigDecimal]) =
//      if (!dynamic) data.grouped(size).map(_.sum / size).toSeq
//      else {
//        val payload = data.grouped(parallel + 1).map(_.init).toSeq.flatten
//        val size = data.grouped(parallel + 1).map(_.last).toSeq.head.toInt
//        payload.grouped(size).map(_.sum / size).toSeq
//      }
//
//    override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
//      correlationMetric(yours, golden, 0.9)
//
//    override def testCases = Seq.fill(1000)(dataType.random)
//
//    override def latency = size - 1 // FOR static only
//  }
//
//  override def inputTypes =
//    if (!dynamic) Seq.fill(parallel)(dataType)
//    else {
//      val sizeType = NumericTypeNew.U(log2Up(size + 1))
//      Seq.fill(parallel)(dataType) :+ sizeType
//    }
//
//  override def outputTypes = Seq.fill(parallel)(retType)
//
//  override def vivadoUtilEstimation = VivadoUtil()
//
//  override def fmaxEstimation = 600e6
//
//  /** --------
//   * implementations
//   * -------- */
//  override def implH = new ChainsawDspModule(this) {
//
//    if (!dynamic) {
//
//    } else {
//      val control = dataIn.last.asUInt()
//      val instantSum = dataIn.init
//    }
//
//  }
//}
