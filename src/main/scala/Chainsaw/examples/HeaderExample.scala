package Chainsaw.examples

import Chainsaw.DataUtil
import Chainsaw.memory.DynamicCounterFreeRun
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

case class Adc() extends Component {

  val period  = in UInt (13 bits)
  val dataOut = out Vec (UInt(14 bits), 2)
  val pulse   = out Bool ()
  val counter = DynamicCounterFreeRun(period)

  dataOut(0) := (counter.value  << 1)
  dataOut(1) := ((counter.value << 1) + U(1))
//  pulse      := counter.value === Random.nextInt(256) // pulse could appear at any time, but always periodically
  pulse := counter.value === 0
}

case class Gps() extends Component {
  val gpsInfo = out Bits (32 bits)
  gpsInfo := B(BigInt("FFFF0123", 16))
}

case class DataPath(bitCount: Int) extends Component {

  val dataIn  = in Vec (UInt(14 bits), 2) // data from ADC
  val header  = in Bits (32 bits)         // header
  val gpsInfo = in Bits (32 bits)         // information from GPS module
  val pulse   = in Bool ()

  val dataOut = master Flow Bits(32 bits) // data to PCIe

  // data packing
  val dataInTruncated                   = dataIn.map(_.takeLow(bitCount))
  val Seq(phase0, phase1)               = dataInTruncated
  val Seq(phase0Delayed, phase1Delayed) = dataInTruncated.map(_.d())

  // for 8-bit
  val ctrlCounter = bitCount match {
    case 8  => CounterFreeRun(2)
    case 12 => CounterFreeRun(4)
  }
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
  when(pulse & ~packDone)(pulseReg.set())
  when(packDone)(pulseReg.clear())
  streamIn.last := packDone & (pulse | pulseReg)

  assert(~(streamIn.last & ~streamIn.valid))

  val FlowOut = streamIn.queue(32).insertHeader(gpsInfo).m2sPipe().insertHeader(header).m2sPipe().toFlow

  dataOut.payload := FlowOut.fragment
  dataOut.valid   := FlowOut.valid
}

case class Top(bitCount: Int) extends Component {

  val period  = in UInt (13 bits)
  val dataOut = out(Flow(Bits(32 bits)))

  val gps      = Gps()
  val adc      = Adc()
  val dataPath = DataPath(bitCount)

  adc.period := period

  dataPath.pulse   := adc.pulse
  dataPath.header  := B(BigInt("FFFFABCD", 16))
  dataPath.gpsInfo := gps.gpsInfo
  dataPath.dataIn  := adc.dataOut

  dataPath.dataOut >> dataOut

  // debug
  val debug = dataOut.payload.takeLow(8)

  debug.simPublic()
}

object DataPath extends App {

  val pulsePeriod = 1024
  val bitCount    = 12
  val packIn = bitCount match {
    case 8  => 4
    case 12 => 8
  }
  val packOut = bitCount match {
    case 8  => 1
    case 12 => 3
  }
  val periodOut = pulsePeriod / packIn * packOut + 2

  val loopBack0 = 1 - (1                << bitCount)
  val loopBack1 = 1 - (pulsePeriod % (1 << bitCount))
  println(s"periodInUse = $pulsePeriod, periodOut = $periodOut, loopBack0 = $loopBack0, loopBack1 = $loopBack1")

  assert(pulsePeriod % packIn == 0)
  assert(pulsePeriod % 2 == 0)

  SimConfig.withFstWave
    .compile(Top(bitCount))
    .doSim { dut =>
      import dut.clockDomain

      clockDomain.forkStimulus(2)
      dut.period #= pulsePeriod / 2
      clockDomain.waitSampling()

      // unstable
      clockDomain.waitSampling(5 * pulsePeriod)

      val payloadBuffer = ArrayBuffer[BigInt]()
      val time          = 100000
      (0 until time).foreach { _ =>
        if (dut.dataOut.valid.toBoolean) payloadBuffer += dut.dataOut.payload.toBigInt
        clockDomain.waitSampling()
      }

      println("data collection done")

      // check data
      var lastHeader  = -100
      var headerFound = false

      val codedBuffer = ArrayBuffer[BigInt]()

      // header verification
      payloadBuffer.indices.foreach { i =>
        if (payloadBuffer(i) == BigInt("FFFFABCD", 16)) { // check header
          headerFound = true
//          println(s"header found at $i")
          if (lastHeader >= 0) assert(i - lastHeader == periodOut, s"header interval error at $i, ${i - lastHeader}")
          lastHeader = i
          assert(
            payloadBuffer(i + 1) == BigInt("FFFF0123", 16),
            s"GPS info error at $i, ${payloadBuffer(i + 1).toString(16)}"
          )
        } else if (i == lastHeader + 1) {} // pass GPS info
        else {                             // store data for verification
          if (headerFound) codedBuffer += payloadBuffer(i).toBigInt
        }
      }
      assert(headerFound, "header not found")

      println("header verification done")

      val decodedBuffer = ArrayBuffer[BigInt]()

      def unpack(value: BigInt, width: Int, count: Int): Seq[BigInt] = {
        var remained = value
        (0 until count).map { _ =>
          val ret = remained % (BigInt(1) << width)
          remained = remained >> width
          ret
        }
      }

      def pack(values: Seq[BigInt], width: Int) = values.reverse.reduce { (a, b) => (a << width) + b }

      // decoding
      codedBuffer.grouped(packOut).foreach { packet =>
        bitCount match {
          case 8  => decodedBuffer ++= unpack(packet.head, 8, 4)
          case 12 => decodedBuffer ++= unpack(pack(packet, 32), 12, 8)
        }
      }

      var lastValue: BigInt = -100
      println(decodedBuffer.take(100).mkString("\n"))
      decodedBuffer.foreach { value =>
        val diff = value - lastValue
        if (lastValue >= 0) assert(diff == 1 || diff == loopBack0 || diff == loopBack1, s"diff = ${diff}")
        lastValue = value
      }
      println("data verification done")
    }
}
