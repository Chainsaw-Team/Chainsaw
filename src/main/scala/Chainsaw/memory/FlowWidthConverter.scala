package Chainsaw.memory

import Chainsaw._
import Chainsaw.xilinx.VivadoUtil
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps
import scala.util.Random

case class FlowWidthConverter(widthIn: Int, widthOut: Int, widthEle: Int)
    extends ChainsawInfiniteGenerator
    with Duty {

  // based on registers, for small size
  // based on lcm and ping-pong buffer

  override def inputTypes = Seq.fill(widthIn)(NumericType.U(widthEle))

  override def outputTypes = Seq.fill(widthOut)(NumericType.U(widthEle))

  val lcmCount = lcm(widthIn, widthOut).toInt

  override def implH = new ChainsawInfiniteModule(this) {

    val depth = 32

    // buffering dataIn as upstream can't be stopped(is a flow)
    val dataInFifo = StreamFifo(flowIn.payload, depth) // data + last
    val overflow   = out Bool ()
    assert(!overflow)
    flowIn.toStream(overflow) >> dataInFifo.io.push

    /** -------- typical ping-pong logic
      * --------
      */
    val regCount           = lcm(widthIn, widthOut).toInt
    val regsPing, regsPong = Reg(Bits(regCount * widthEle bits))

    val combValid, combReady = Bool()
    val counterIn  = Counter(stateCount = regCount / widthIn, inc = validIn)
    val counterOut = Counter(stateCount = regCount / widthOut, inc = combValid)

    val pingRead, pingWrite = RegInit(False)
    pingWrite.toggleWhen(counterIn.willOverflow)
    pingRead.toggleWhen(counterOut.willOverflow)

    val fsm = new StateMachine {
      // classic ping-pong FSM
      val EMPTY      = makeInstantEntry()
      val HALF, FULL = State()
      // state transition logic
      EMPTY.whenIsActive(when(counterIn.willOverflow)(goto(HALF)))
      HALF.whenIsActive(
        when(counterIn.willOverflow && counterOut.willOverflow)(goto(HALF))
          .elsewhen(counterOut.willOverflow)(goto(EMPTY))
          .elsewhen(counterIn.willOverflow)(goto(FULL))
      )
      FULL.whenIsActive(when(counterOut.willOverflow)(goto(HALF)))
      assert(!(isActive(FULL) && counterIn.willOverflow), "ping-pong overflow")

      // read/write logic by connection

      // write, latency = 0
      combReady               := isActive(HALF) || isActive(EMPTY)
      dataInFifo.io.pop.ready := combReady

      val writeStart = counterIn * widthIn * widthEle
      val writeWidth = widthIn * widthEle bits

      when(validIn) {
        when(pingWrite)(regsPing(writeStart, writeWidth) := dataIn.asBits)
          .otherwise((regsPong(writeStart, writeWidth) := dataIn.asBits))
      }

      // read, latency = 1 for multiplication
      val readLatency = 1
      val readStart   = (counterOut * widthOut * widthEle).d(readLatency)
      val readWidth   = widthOut * widthEle bits

      val regOutPing: Bits = regsPing(readStart, readWidth)
      val regOutPong: Bits = regsPong(readStart, readWidth)
      val ret = Mux(pingRead.d(readLatency), regOutPing, regOutPong)

      // TODO: generate last based on lastIn
      combValid := (isActive(FULL) || isActive(HALF))

      dataOut assignFromBits ret
      validOut := combValid.validAfter(readLatency)

      // TODO: low-cost implementation
      val lastInFifo = StreamFifo(Bool(), depth)
      lastInFifo.io.push.valid   := counterIn.willOverflow
      lastInFifo.io.push.payload := lastIn
      lastInFifo.io.pop.ready :=
        counterOut.willOverflow.d(readLatency) && validOut
      lastOut := lastInFifo.io.pop.payload && lastInFifo.io.pop.fire
    }
  }

  override def implNaiveH = None

  override def name = s"FlowWidthConverter_${widthIn}_${widthOut}_${widthEle}"

  /** -------- performance
    * --------
    */
  override def vivadoUtilEstimation = VivadoUtil()

  override def fmaxEstimation = 600 MHz

  /** -------- model
    * --------
    */
  override def impl(testCase: TestCase) = {
    require(
      testCase.data.length % lcmCount == 0,
      "the length of data should be a multiple of lcmCount, or an unaligned frame will be generated"
    )
    testCase.data
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    ChainsawMetric.sameAsBigInt(yours, golden)

  override def testCases = {
    val lengths =
      Seq.fill(100)(
        (lcmCount / widthIn) * (Random.nextInt(4) + 1)
      ) // assure lcm
    lengths.map(randomTestCase)
  }

  override def resetCycle = 0

  override def dutyRation =
    if (widthIn > widthOut) widthOut.toDouble / widthIn else 1
}
