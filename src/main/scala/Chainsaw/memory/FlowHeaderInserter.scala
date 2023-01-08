package Chainsaw.memory

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** insert a header before each sequence
  */
case class FlowHeaderInserter(header: Seq[BigInt], width: Int)
    extends ChainsawCustomModule {

  // TODO: make this a ChainsawGenerator

  override def inputTypes = Seq(NumericType.U(width))

  override def outputTypes = Seq(NumericType.U(width))

  // buffering dataIn as upstream can't be stopped(is a flow)
  val dataInFifo = StreamFifo(flowIn.payload, 32)
  dataInFifo.io.push.payload := flowIn.payload

  val overflow = out Bool ()
  overflow := !dataInFifo.io.push.ready && validIn

  val headerRom = Mem(header.reverse.map(B(_, width bits)))

  val fsm = new StateMachine {
    val WAITHEADER = makeInstantEntry() // for sync
    val HEADER     = new StateDelay(header.length)
    val DATA       = State()
    WAITHEADER.whenIsActive {
      dataInFifo.io.push.valid := False // ban fifo from pushing
      dataInFifo.io.pop.ready  := False
      validOut                 := False
      dataOut.head.assignDontCare()
      when(lastIn)(goto(HEADER))
    }
    HEADER.whenIsActive {
      dataInFifo.io.push.valid := validIn
      dataInFifo.io.pop.ready  := False
      validOut                 := True

      dataOut.head assignFromBits headerRom.readAsync(HEADER.cache.value)
      goto(DATA)
    }
    DATA.whenIsActive {
      dataInFifo.io.push.valid := validIn
      dataInFifo.io.pop.ready  := True
      validOut                 := dataInFifo.io.pop.valid
      dataOut                  := dataInFifo.io.pop.payload.fragment
      when(dataInFifo.io.pop.payload.last)(goto(HEADER))
    }

    lastOut := dataInFifo.io.pop.payload.last && dataInFifo.io.pop.fire
  }
}

object FlowHeaderInserter extends App {
  import scala.util.Random

  val header = Seq(BigInt(0x0f0f1f1f))

  SimConfig.withFstWave
    .compile(FlowHeaderInserter(header, width = 32))
    .doSim { dut =>
      dut.validIn #= false
      dut.lastIn  #= true
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()

      val seq0 = Seq.fill(10)(BigInt(32, Random))
      val seq1 = Seq.fill(20)(BigInt(32, Random))
      val seq2 = Seq.fill(30)(BigInt(32, Random))

      val last0 = Seq.fill(9)(false) :+ true
      val last1 = Seq.fill(19)(false) :+ true
      val last2 = Seq.fill(29)(false) :+ true

      val buffer = ArrayBuffer[(BigInt, Boolean)]()
      Seq(seq0, seq1, seq2).zip(Seq(last0, last1, last2)).foreach {
        case (ints, booleans) => ints.zip(booleans).copyToBuffer(buffer)
      }

      val record    = ArrayBuffer[BigInt]()
      val timeCount = 100

      (0 until timeCount).foreach { i =>
        if (buffer.nonEmpty) {
          dut.validIn #= true
          val current = buffer.remove(0)
          dut.dataIn.head.raw #= current._1
          dut.lastIn          #= current._2
        } else {
          dut.validIn #= false
          dut.lastIn  #= false
        }
        dut.clockDomain.waitSampling()
        if (dut.validOut.toBoolean) record += dut.dataOut.head.raw.toBigInt
      }

      def checkHeader(data: Seq[BigInt]): Unit = {
        assert(data.zip(header).forall { case (int, i) => int == i })
      }

      record.foreach(int => println(int.toString(16)))

      val out0 = record.slice(0, 0 + 11)
      val out1 = record.slice(11, 11 + 21)
      val out2 = record.slice(32, 32 + 31)
      Seq(out0, out1, out2).foreach(checkHeader)

    }
}
