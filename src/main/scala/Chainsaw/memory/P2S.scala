package Chainsaw.memory

import Chainsaw._
import spinal.core._
import spinal.lib._
import Chainsaw._
import Chainsaw.xilinx._

import scala.language.postfixOps

case class P2S(p: Int, s: Int, bitWidth: Int)
    extends ChainsawFrameGenerator
    with FixedLatency {
  require(p % s == 0)

  override def name = s"P2S_s${s}_p${p}_w$bitWidth"

  override def vivadoUtilEstimation =
    VivadoUtil(ff = lcm(p, s).toInt * bitWidth)

  override def fmaxEstimation = 600 MHz

  override def inputTypes = Seq.fill(p)(NumericType.U(bitWidth))

  override def outputTypes = Seq.fill(s)(NumericType.U(bitWidth))

  override def impl(testCase: TestCase) = testCase.data

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.equals(golden)

  override def testCases = Seq.fill(100)(TestCase(randomDataVector))

  override def resetCycle = 0

  val regCount = lcm(p, s).toInt
  // reg & MUX-based implementation

  override def implH = new ChainsawFrameModule(this) {
    val counter = CounterFreeRun(p / s)
    when(!validIn)(counter.clear()) // reset inner state during interrupt

    // write
    val segments =
      dataIn.grouped(s).toSeq.map(_.asBits()) // merge elements before mux
    val buffers =
      segments.tail.map(segment => RegNextWhen(segment, counter.value === 0))

    // read
    val ret = Bits(outputTypes.map(_.bitWidth).sum bits)
    switch(counter.value) {
      is(U(0))(ret := segments.head)
      (1 until p / s).foreach(i => is(U(i))(ret := buffers(i - 1)))
      if (!isPow2(p / s)) default(ret.assignDontCare())
    }

    dataOut := ret
      .d(1) // registered output
      .subdivideIn(bitWidth bits)
      .map(_.asUInt.toAFix) // split elements
    lastOut := lastIn.validAfter(latency())
  }

  override def implNaiveH = None

  override def inputFrameFormat = MatrixFormatAddBubble(p, 1, p / s - 1)

  override def outputFrameFormat = MatrixFormat(s, p / s)

  override def latency() = 1
}
