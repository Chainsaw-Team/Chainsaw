//package Chainsaw.dsp
//
//import Chainsaw._
//import Chainsaw.xilinx._
//import spinal.core._
//import spinal.lib._
//
//case class MovingAverage(size: Int, dynamic: Boolean,
//                         dataType: NumericTypeNew, parallel: Int)
//  extends ChainsawDspGenerator {
//
//  val retType = Seq.fill(size)(dataType).reduce(_ + _)
//
//  override def name = getAutoName(this)
//
//  val muxLatency = if (dynamic) 1 else 0
//  val sumLatency = log2Up(size)
//  val multLatency = 2
//  val dataPathLatency = muxLatency + sumLatency + multLatency
//
//  override def model = new ChainsawInfiniteModel {
//
//    override def impl(data: Seq[BigDecimal]) = {
//      if (!dynamic) data.sliding(size).map(_.sum / size).toSeq
//      else {
//        // TODO: dynamic model which change along with the control value
//        val payload: Seq[BigDecimal] = data.grouped(parallel + 1).map(_.init).toSeq.flatten
//        val size: Int = data.grouped(parallel + 1).map(_.last).toSeq.head.toInt
//        payload.sliding(size).map(_.sum / size).toSeq
//      }
//    }
//
//    override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
//      correlationMetric(yours, golden, 0.9)
//
//    override def testCases =
//    // TODO: dynamic model which provide different control values over time
//      if (!dynamic) Seq.fill(1000)(dataType.random)
//      else Seq.fill(1000)((Seq.fill(parallel)(dataType.random) :+ BigDecimal(size / 3))).flatten
//
//    override def latency = dataPathLatency + (size.divideAndCeil(parallel) - 1)
//
//    override def offset = 0
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
//  /** --------
//   * performance
//   * -------- */
//  override def vivadoUtilEstimation = VivadoUtilEstimation()
//
//  override def fmaxEstimation = 600e6
//
//  override def implH = new ChainsawDspModule(this) {
//
//    def controlIn =
//      if (!dynamic) throw new IllegalAccessError("control is not defined for static MovingAverage")
//      else dataIn.last
//
//    val sizeInUse = size + parallel - 1
//    val historyLengths = sizeInUse.divideToChannel(parallel)
//    val fullLength = sizeInUse.divideAndCeil(parallel)
//    val historiesByChannels =
//      dataIn.take(parallel).zip(historyLengths).map { case (data, length) =>
//        History(data, length).padTo(fullLength, null)
//      }
//    val history = historiesByChannels.transpose.flatten.take(sizeInUse)
//    val control = dataIn.last.asUInt()
//
//    // TODO: take advantage of the common part of the history
//    val elementsByChannels = {
//      if (!dynamic) history.sliding(size).toSeq else {
//        val mapping = (0 until nextPow2(size + 1).toInt)
//          .map(i => (i,
//            if (i >= size) B("1" * size)
//            else B("1" * i + "0" * (size - i))))
//        val valids = control.muxList(mapping).d()
//        history.sliding(size).map(his => his.zip(valids.asBools).map { case (ele, bool) => Mux(bool, ele, ele.getZero).d() }).toSeq
//      }
//    }
//
//    def pipeline(ele: AFix, i: Int) = ele.d()
//
//    val sums = elementsByChannels
//      .map(_
//        .reduceBalancedTree(_ + _, pipeline))
//
//    val scalingFactorType = NumericTypeNew(integral = 0, fractional = 17, signed = true)
//    val scalingFactor = if (!dynamic) scalingFactorType.fromConstant(1 / size.toDouble)
//    else {
//      val scalingFactorRom = Mem((Seq(2.0, 2.0) ++ (2 to size).map(_.toDouble)).map(i => scalingFactorType.fromConstant(1 / i)))
//      scalingFactorRom.readSync(control)
//    }
//    dataOut := sums.map(sum => (sum * scalingFactor).d(2).fixTo(retType()))
//
//    val dataPathLatencyInUse = LatencyAnalysis(dataIn.head.raw, dataOut.head.raw)
//    assert(dataPathLatencyInUse == dataPathLatency, s"latency mismatch, yours: $dataPathLatency, true: $dataPathLatencyInUse")
//
//    if(!dynamic) doControlByLatency()
//    else doControlByLatency() // FIXME: do control for dynamic MA by valids running along with history
//  }
//}
