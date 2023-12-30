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
  val BITWIDTH = 12

  val busArea = new ClockingArea(busClockDomain) {
    // initialize cross clock domain registers, all "points" means number of clock cycles
    val selfTest = busIf.newReg("selfTest").field(Bool(), AccessType.RW, 0, "self-test flag")
    val header   = busIf.newReg("header").field(word(), AccessType.RW, HEADER, "frame header")
  }
  val selfTest = getControlData(busArea.selfTest)
  val header   = getControlData(busArea.header)


  val counter   = CounterFreeRun(1 << (BITWIDTH - 1))
  val padding   = 14 - BITWIDTH
  val testData0 = (counter.value  << 1) @@ U(0, padding bits)
  val testData1 = ((counter.value << 1) + U(1)) @@ U(0, padding bits)

  // data selection
  val dataInUse = Mux(selfTest, Vec(testData0, testData1), dataIn).d()

  // data selection
  val dataInTruncated                   = dataInUse.map(_.takeHigh(BITWIDTH))
  val Seq(phase0, phase1)               = dataInTruncated
  val Seq(phase0Delayed, phase1Delayed) = dataInTruncated.map(_.d())

  val packCounter = CounterFreeRun(4)
  when(pulseRise) { packCounter.clear() }

  val packedData = Bits(32 bits)
  switch(packCounter.value) {
    is(0)(packedData.assignDontCare())
    is(1)(packedData := phase0.takeLow(8) ## phase1Delayed ## phase0Delayed)
    is(2)(packedData := phase1.takeLow(4) ## phase0 ## phase1Delayed ## phase0Delayed.takeHigh(4))
    is(3)(packedData := phase1 ## phase0 ## phase1Delayed.takeHigh(8))
  }

  val packDone    = packCounter.willOverflow
  val packedValid = ~(packCounter.value === U(0))

  // "move" the pulse to the next packDone, avoid the header from splitting any packet
  val pulseReg = RegInit(False)
  when(pulseRise & ~packDone)(pulseReg.set())
  when(packDone)(pulseReg.clear())
  val packedLast = packDone & (pulseRise | pulseReg)

  // preparing test data
  val testCounter = CounterFreeRun(BigInt(1) << 32)
  val testData    = testCounter.value.asBits
  when(packedLast)(testCounter.clear())

  val streamIn = Stream(Fragment(Bits(32 bits)))
  streamIn.fragment := packedData.d()
  streamIn.valid    := packedValid.d()
  streamIn.last     := packedLast.d()

  val withGps = streamIn.queue(32).insertHeader(gpsInfo).m2sPipe()
  val FlowOut = withGps.insertHeader(header.asBits).m2sPipe().toFlow

  dataOut.payload := FlowOut.fragment
  dataOut.valid   := FlowOut.valid

  // debug
  assert(~(streamIn.last & ~streamIn.valid))
  packDone.setName("packDone")
  pulseRise.setName("pulseRise")
  pulseReg.setName("pulseDelayed")
  streamIn.last.setName("frameLast")
  streamIn.valid.setName("frameValid")
}
