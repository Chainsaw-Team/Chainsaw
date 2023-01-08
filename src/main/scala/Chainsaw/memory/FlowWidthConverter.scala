package Chainsaw.memory

import Chainsaw._
import Chainsaw.intel.QuartusFlow
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

// TODO: make this a ChainsawFrameGenerator
case class FlowWidthConverter(widthIn: Int, widthOut: Int)
    extends ChainsawCustomModule {

  override def inputTypes = Seq(NumericType.U(widthIn))

  override def outputTypes = Seq(NumericType.U(widthOut))

  val overflow = out Bool ()

  // based on registers, for small size
  val regCount           = lcm(widthIn, widthOut).toInt // todo: use lcm
  val regsPing, regsPong = Reg(Bits(regCount bits))

  val dataInFifo = StreamFifo(flowIn.payload, 32)
  // buffering dataIn as upstream can't be stopped(is a flow)
  flowIn.toStream(overflow) >> dataInFifo.io.push

  val counterIn =
    Counter(stateCount = regCount / widthIn, inc = validIn)
  val combValid, combReady = Bool()
  val counterOut =
    Counter(stateCount = regCount / widthOut, inc = combValid)
  val ping = RegInit(False)
  ping.toggleWhen(counterIn.willOverflow)

  val fsm = new StateMachine { // classic ping-pong FSM
    val EMPTY      = makeInstantEntry()
    val HALF, FULL = State()
    // state transition logic
    EMPTY.whenIsActive(when(counterIn.willOverflow)(goto(HALF)))
    HALF.whenIsActive(
      when(counterIn.willOverflow && counterOut.willOverflow)(goto(HALF))
        .elsewhen(counterOut.willOverflow)(goto(EMPTY))
        .elsewhen(counterIn.willOverflow)(goto(FULL))
    )
    FULL.whenIsActive(when(counterOut.willOverflow)(goto(EMPTY)))

    // read/write logic by connection

    // write, latency = 0
    combReady               := isActive(HALF) || isActive(EMPTY)
    dataInFifo.io.pop.ready := combReady
    when(validIn) {
      when(ping)(regsPing(counterIn * widthIn, widthIn bits) := dataIn.asBits)
        .otherwise(regsPong(counterIn * widthIn, widthIn bits) := dataIn.asBits)
    }

    // read, latency = 1 for multiplication
    combValid := (isActive(FULL) || isActive(HALF))
    val readLatency = 1
    validOut := combValid.validAfter(readLatency)
    val regOutPing: Bits =
      regsPing((counterOut * widthOut).d(readLatency), widthOut bits)
    val regOutPong: Bits =
      regsPong((counterOut * widthOut).d(readLatency), widthOut bits)
    dataOut.head assignFromBits Mux(ping.d(readLatency), regOutPong, regOutPing)

    lastOut := counterOut.willOverflow.d(readLatency) && validOut
    assert(!overflow)
  }
}

object FlowWidthConverter extends App {
  import scala.util.Random

  // verification
  val widthIn  = 28
  val widthOut = 32
  SimConfig.withFstWave.compile(FlowWidthConverter(28, 32)).doSim { dut =>
    dut.validIn #= false
    dut.lastIn  #= false
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()

    val dataCount  = 16 * 100
    val timeCount  = dataCount * 4 / 3 + 40
    val data       = Seq.fill(dataCount)(BigInt(widthIn, Random))
    val dataBuffer = ArrayBuffer[BigInt]()
    val record     = ArrayBuffer[BigInt]()

    data.copyToBuffer(dataBuffer)
    (0 until timeCount).foreach { i =>
      if (dataBuffer.nonEmpty && i % 4 != 1) { // 3/4 occupation
        dut.validIn         #= true
        dut.dataIn.head.raw #= dataBuffer.remove(0)
      } else {
        dut.validIn         #= false
        dut.dataIn.head.raw #= BigInt(widthIn, Random)
      }
      dut.clockDomain.waitSampling()
      if (dut.validOut.toBoolean) {
        record += dut.dataOut.head.raw.toBigInt
      }
    }

    assert(
      record.length == dataCount * 7 / 8,
      s"record length ${record.length}"
    )
    val golden = data.reverse.map(_.toBitValue(widthIn)).reduce(_ @@ _).value
    val yours  = record.reverse.map(_.toBitValue(widthOut)).reduce(_ @@ _).value
    assert(yours == golden, s"${yours.toString(16)} != ${golden.toString(16)}")
  }

  //
  new QuartusFlow(FlowWidthConverter(28, 32)).impl()
}
