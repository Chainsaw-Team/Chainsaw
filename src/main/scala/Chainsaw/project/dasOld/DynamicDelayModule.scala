package Chainsaw.project.das

import Chainsaw._
import Chainsaw.intel.QuartusFlow
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.regif._

import scala.language.postfixOps

// TODO: 更完整的正确性测试

case class DynamicDelay(delayMax: Int, payloadIn: Bits, delay: Option[UInt], bus: Option[BusIf]) extends Area {

  val delayWidth = log2Up(delayMax + 1)
  val delayInUse = bus match {
    case Some(value) =>
      val reg = value.newReg("delay")
      reg.field(UInt(log2Up(delayMax + 1) bits), AccessType.RW)
    case None =>
      delay match {
        case Some(value) =>
          assert(delay.get.getBitsWidth == delayWidth, "")
          delay.get
        case None => throw new IllegalArgumentException("")
      }
  }

  val payloadOut = Bits(payloadIn.getBitsWidth bits)
  val stable = Bool()

  val counterWidth = delayWidth
  val paddedLength = pow2(counterWidth)

  // delay datapath
  val ram = Mem(HardType(payloadIn), paddedLength)
  val writeAddrCounter = CounterFreeRun(paddedLength)
  // ram latency = +2 for read latency, +1 for address latency
  val readAddr = (writeAddrCounter.value + 3 - delayInUse).d()
  ram.write(writeAddrCounter.value, payloadIn)
  val payloadHistory = History(payloadIn, 3) // for delay < 3

  switch(delayInUse) {
    (0 until 3).foreach(i => is(U(i))(payloadOut := payloadHistory(i)))
    default(payloadOut := ram.readSync(readAddr).d())
  }

  // stable datapath
  val countDownValue = Reg(UInt(delayWidth bits))
  stable := countDownValue === countDownValue.getZero || delayInUse === delayInUse.getZero
  when(delayInUse =/= delayInUse.d())(countDownValue := delayInUse - U(1))
    .elsewhen(!stable)(countDownValue := (countDownValue - U(1)))

}

case class DynamicDelayModule(delayMax: Int, dataWidth: Int) extends Module {

  val delayWidth = log2Up(delayMax + 1)
  val delay = in UInt (delayWidth bits)
  val payloadIn = in Bits (dataWidth bits)
  val payloadOut = out Bits (dataWidth bits)
  val stable = out Bool()

  val area = DynamicDelay(delayMax, payloadIn, Some(delay), None)
  payloadOut := area.payloadOut
  stable := area.stable
}

object DynamicDelayModule extends App {

  SimConfig.withFstWave.compile(DynamicDelayModule(16, 8)).doSim { dut =>

    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()

    (0 until 100).foreach { i =>
      dut.delay #= 16
      dut.payloadIn #= i
      dut.clockDomain.waitSampling()
    }

    (0 until 100).foreach { i =>
      dut.delay #= 0
      dut.payloadIn #= i
      dut.clockDomain.waitSampling()
    }

    (0 until 100).foreach { i =>
      dut.delay #= 3
      dut.payloadIn #= i
      dut.clockDomain.waitSampling()
    }
  }

  new QuartusFlow(dut = DynamicDelayModule(1024, 16), "DyanmicDelay").impl()

}
