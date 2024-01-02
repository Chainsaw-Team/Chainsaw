package Chainsaw.project.das

import Chainsaw.{DataUtil, VecUtil}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.{AccessType, BusIf}

import scala.language.postfixOps

/** The data path for processing ADC data, including poly-phase decomposition, parallel to serial conversion, data
  * packing and framing
  *
  * @param busIf
  *   The Xillybus mem bus interface.
  * @param busClockDomain
  *   The 125MHz clock domain for the Xillybus.
  * @param lvdsClockDomain
  *   The 62.5MHz clock domain for the LVDS data.
  * @param lvdsDataIn
  *   The LVDS data input.
  * @param pulseRise
  *   A boolean flag indicating the beginning of a pulse.
  * @param gpsInfo
  *   Time information from the GPS module.
  * @param dataOut
  *   The output stream of processed data.
  */
case class DataPath(
    busIf: BusIf,
    busClockDomain: ClockDomain,
    lvdsClockDomain: ClockDomain,
    lvdsDataIn: Adc62_5,
    pulseRise: Bool,
    gpsInfo: Bits, // information from GPS module
    dataOut: Stream[Bits]
) extends Area {

  // parameter
  val HEADER         = 0x40c040c0L
  val ADCBITWIDTH    = 14
  val UPLOADBITWIDTH = 12

  ////////////////////
  // initialize cross clock domain registers
  ////////////////////
  val busArea = new ClockingArea(busClockDomain) {
    val selfTest = busIf.newReg("selfTest").field(Bool(), AccessType.RW, 0, "self-test flag")
    val header   = busIf.newReg("header").field(word(), AccessType.RW, HEADER, "frame header")
    val useChannel0 =
      busIf.newReg("useChannel0").field(Bool(), AccessType.RW, 1, "data source control, 0 for port 1,2, 1 for port 3,4")
  }
  val selfTest    = getControlData(busArea.selfTest)
  val header      = getControlData(busArea.header)
  val useChannel0 = getControlData(busArea.useChannel0)

  ////////////////////
  // test data generation
  ////////////////////
  val lvdsArea = new ClockingArea(lvdsClockDomain) {
    val counter = CounterFreeRun(1 << (UPLOADBITWIDTH - 2))
    val padding = ADCBITWIDTH - UPLOADBITWIDTH
    val testData = (0 until 4)
      .map { i => // 0,1,2,3, corresponding to D,C,B,A
        ((counter.value << 2) + U(i)) @@ U(0, padding bits)
      }
      .map(_.asBits)
      .map(_.d())
  }

  val testData = Vec(lvdsArea.testData)
  // data order in time is D,C,B,A
  val adcData0 = Vec(lvdsDataIn.DOUTD, lvdsDataIn.DOUTC, lvdsDataIn.DOUTB, lvdsDataIn.DOUTA)
  val adcData1 = Vec(lvdsDataIn.DOUTBD, lvdsDataIn.DOUTBC, lvdsDataIn.DOUTBB, lvdsDataIn.DOUTBA)

  ////////////////////
  // poly-phase & parallel to serial conversion
  ////////////////////
  def p2s(vec: Vec[Bits]): Bits = {
    val streamParallelSlow = Stream(Bits(ADCBITWIDTH * 2 bits))
    val streamSerial       = Stream(Bits(ADCBITWIDTH bits))

    streamParallelSlow.valid   := True
    streamParallelSlow.payload := vec.reverse.reduce(_ ## _)
//    streamParallelSlow.payload := vec.reduce(_ ## _)

    val streamParallelFast = streamParallelSlow.queue(8, lvdsClockDomain, ClockDomain.current)
    StreamWidthAdapter(streamParallelFast, streamSerial, endianness = LITTLE)

    streamSerial.m2sPipe().freeRun().payload // pipelined output
  }
  val channels: Seq[Vec[Bits]] = adcData0.groupByChannel(2) ++ adcData1.groupByChannel(2) ++ testData.groupByChannel(2)
  val Seq(adc0X0S, adc0X1S, adc1X0S, adc1X1S, testData0, testData1) = channels.map(p2s)

  ////////////////////
  // channel selection
  ////////////////////
  val dataSelected = Vec(Bits(ADCBITWIDTH bits), 2)
  dataSelected(0) := Mux(useChannel0, adc0X0S, adc1X0S).d()
  dataSelected(1) := Mux(useChannel0, adc0X1S, adc1X1S).d()

  ////////////////////
  // data selection
  ////////////////////
  val dataInUse = Mux(selfTest, Vec(testData0, testData1), dataSelected).d()

  ////////////////////
  // data packing
  ////////////////////
  val dataInTruncated                   = dataInUse.map(_.takeHigh(UPLOADBITWIDTH))
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

  ////////////////////
  // framing(header insertion)
  ////////////////////
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
