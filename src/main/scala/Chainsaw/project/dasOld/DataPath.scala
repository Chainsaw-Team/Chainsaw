package Chainsaw.project.das

import Chainsaw.DataUtil
import spinal.core._
import spinal.lib._

case class DataPath(bitCount: Int) extends Component {

  val dataIn   = in Vec (UInt(14 bits), 2) // data from ADC
  val pulseIn  = in Bool ()
  val header   = in Bits (32 bits)         // header
  val gpsInfo  = in Bits (32 bits)         // information from GPS module
  val selfTest = in Bool ()
  val dataOut  = master Flow Bits(32 bits) // data to PCIe

  // self-test module

  val counter = CounterFreeRun(1 << (bitCount - 1))
  val padding = 14 - bitCount

  val testData0 = (counter.value  << 1) @@ U(0, padding bits)
  val testData1 = ((counter.value << 1) + U(1)) @@ U(0, padding bits)

  // data selection
  val dataInUse = Mux(selfTest, Vec(testData0, testData1), dataIn).d()
//  val pulseIn                        = Mux(selfTest, testPulse, pulseIn).d()
  val dataInTruncated                   = dataInUse.map(_.takeHigh(bitCount))
  val Seq(phase0, phase1)               = dataInTruncated
  val Seq(phase0Delayed, phase1Delayed) = dataInTruncated.map(_.d())

  // data packing
  // for 8-bit
  val ctrlCounter = bitCount match {
    case 8  => CounterFreeRun(2)
    case 12 => CounterFreeRun(4)
  }
  when(pulseIn)(ctrlCounter.clear()) // 保持多次寄存器update之间,脉冲头与数据帧头间的偏差稳定
  val packDone   = ctrlCounter.willOverflowIfInc
  val packedFlow = Flow(Bits(32 bits))

  bitCount match {
    case 8 =>
      packedFlow.payload := (phase1 ## phase0 ## phase1Delayed ## phase0Delayed)
      packedFlow.valid   := ~(ctrlCounter.value === U(0))
    case 12 =>
      packedFlow.valid := ~(ctrlCounter.value === U(0))
      switch(ctrlCounter.value) {
        is(0)(packedFlow.payload.assignDontCare())
        is(1)(packedFlow.payload := phase0.takeLow(8) ## phase1Delayed ## phase0Delayed)
        is(2)(packedFlow.payload := phase1.takeLow(4) ## phase0 ## phase1Delayed ## phase0Delayed.takeHigh(4))
        is(3)(packedFlow.payload := phase1 ## phase0 ## phase1Delayed.takeHigh(8))
      }
  }

  val streamIn = Stream(Fragment(Bits(32 bits)))
  streamIn.fragment := packedFlow.payload
  streamIn.valid    := packedFlow.valid

  // "move" the pulse to the next packDone, avoid the header from splitting any packet
  val pulseReg = RegInit(False)
  when(pulseIn & ~packDone)(pulseReg.set())
  when(packDone)(pulseReg.clear())
  streamIn.last := packDone & (pulseIn | pulseReg)

  assert(~(streamIn.last & ~streamIn.valid))

  val withGps = streamIn.queue(32).insertHeader(gpsInfo).m2sPipe()
  val FlowOut = withGps.insertHeader(header).m2sPipe().toFlow

  dataOut.payload := FlowOut.fragment
  dataOut.valid   := FlowOut.valid
}
