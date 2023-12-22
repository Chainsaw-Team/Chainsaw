package Chainsaw.project.das

import Chainsaw.DataUtil
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.{AccessType, BusIf}

case class DataPath(
    busIf: BusIf,
    busClockDomain: ClockDomain,
    dataIn: Vec[UInt], // data from ADC
    pulseRise: Bool,
    gpsInfo: Bits, // information from GPS module
    dataOut: Stream[Bits]
) extends Area {

  // initialize cross clock domain registers
  val HEADER = 0x40c040c0L
  val busArea = new ClockingArea(busClockDomain) {
    // initialize cross clock domain registers, all "points" means number of clock cycles
    val selfTest = busIf.newReg("selfTest").field(Bool(), AccessType.RW, 0, "self-test flag")
    val header   = busIf.newReg("header").field(word(), AccessType.RW, HEADER, "frame header")
  }
  val selfTest = getControlData(busArea.selfTest)
  val header   = getControlData(busArea.header)

  // self-test data generation
  val counter   = CounterFreeRun(1 << (dataWidth - 1))
  val padding   = 14 - dataWidth
  val testData0 = (counter.value  << 1) @@ U(0, padding bits)
  val testData1 = ((counter.value << 1) + U(1)) @@ U(0, padding bits)

  // data selection
  val dataInUse                         = Mux(selfTest, Vec(testData0, testData1), dataIn).d()
  val dataInTruncated                   = dataInUse.map(_.takeHigh(dataWidth))
  val Seq(phase0, phase1)               = dataInTruncated
  val Seq(phase0Delayed, phase1Delayed) = dataInTruncated.map(_.d())

  // data packing
  // for 8-bit
  val ctrlCounter = dataWidth match {
    case 8  => CounterFreeRun(2)
    case 12 => CounterFreeRun(4)
  }
  when(pulseRise)(ctrlCounter.clear()) // 保持多次寄存器update之间,脉冲头与数据帧头间的偏差稳定
  val packDone   = ctrlCounter.willOverflowIfInc
  val packedFlow = Flow(Bits(32 bits))

  dataWidth match {
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
  when(pulseRise & ~packDone)(pulseReg.set())
  when(packDone)(pulseReg.clear())
  streamIn.last := packDone & (pulseRise | pulseReg)

  assert(~(streamIn.last & ~streamIn.valid))

  val withGps = streamIn.queue(32).insertHeader(gpsInfo).m2sPipe()
  val FlowOut = withGps.insertHeader(header.asBits).m2sPipe().toFlow

  dataOut.payload := FlowOut.fragment
  dataOut.valid   := FlowOut.valid

  // debug
  packDone.setName("packDone")
  pulseRise.setName("pulseRise")
  pulseReg.setName("pulseDelayed")
  streamIn.last.setName("frameLast")
  streamIn.valid.setName("frameValid")
}
