package Chainsaw.deprecated

import Chainsaw.logger
import spinal.core._
import spinal.lib._
import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class ChainsawModule(val gen: ChainsawGenerator)
  extends Module {

  import gen._

  /** --------
   * I/O
   * -------- */
  val flowIn = slave Flow Fragment(Vec(inputWidths.map(w => Bits(w bits))))
  val flowOut = master Flow Fragment(Vec(outputWidths.map(w => Bits(w bits))))

  val dataIn = flowIn.fragment
  val validIn = flowIn.valid
  val lastIn = flowIn.last

  val dataOut = flowOut.fragment
  val validOut = flowOut.valid
  val lastOut = flowOut.last

  // TODO: name generator
  setDefinitionName(gen.name)

  /** --------
   * auto-implementation of validOut and lastOut
   * -------- */
  // TODO: optimization

  // as a lazy val, the counter won't be created if you never use it,
  // besides, it won't be created multiple times even if you repeatedly use it
  val localCounter = {
    val ret = Counter(period)
    when(validIn)(ret.increment())
    ret.setName("localCounter")
    ret
  }

  lastOut := lastIn.validAfter(latency)
  validOut := validIn.validAfter(latency)

  // FIXME: assertion works before reset, why?

  //  if (atSimTime) { // assertion for valid input which is continuous
  //
  //    val workingCounter = Counter(period)
  //    when(validIn)(workingCounter.increment())
  //    workingCounter.setName("workingCounter")
  //
  //    val fsm: StateMachine = new StateMachine {
  //
  //      val validNext = validIn.validAfter(1)
  //      val lastNext = lastIn.validAfter(1)
  //
  //      val WAITING = makeInstantEntry()
  //      val WORKING0, WORKING1 = new StateDelay(period)
  //      WAITING.whenIsActive(when(validIn)(goto(WORKING0)))
  //      WORKING0.whenCompleted {
  //        when(validIn)(goto(WORKING1)).otherwise(goto(WAITING))
  //        assert(lastNext, "illegal input: lastIn is expected at the end of a frame", WARNING)
  //      }
  //      WORKING1.whenCompleted {
  //        when(validIn)(goto(WORKING0)).otherwise(goto(WAITING))
  //        assert(lastNext, s"illegal input: lastIn is expected at the end of a frame at time", WARNING)
  //      }
  //      val badValid0 = isActive(WORKING0) && !validNext
  //      val badValid1 = isActive(WORKING1) && !validNext
  //      assert(!badValid0, s"input must be continuous within a frame", WARNING)
  //      assert(!badValid1, s"input must be continuous within a frame", WARNING)
  //    }
  //    fsm.setName("workingStateFsm")
  //  }

  /** --------
   * control utils
   * -------- */
  def betweenTime(from: Int, until: Int) = {
    require(until - from <= period, "betweenTime can't be used to specify a range longer than the period")
    if (until >= 128) logger.warn(s"betweenTime is implemented by delay line, delay line longer than $until may not suitable for efficiency, you'd better design the control logic by yourself")
    //    localCounter.value >= from && localCounter.value < until
    val mark = RegInit(False)
    val aMark = lastIn.validAfter(from)
    val bMark = lastIn.validAfter(until)
    when(aMark && bMark) {} // this may also happen
      .elsewhen(aMark)(mark.set())
      .elsewhen(bMark)(mark.clear())
    mark
  }

  def atTime(time: Int) = localCounter.value === U(time)

  def beforeTime(time: Int) = betweenTime(0, time)

  def afterTime(time: Int) = betweenTime(time + 1, period)

  def delayedValid(delay: Int) = validIn.validAfter(delay)

  def delayedLast(delay: Int) = lastIn.validAfter(delay)

  /** --------
   * numeric utils
   * -------- */
  lazy val complexDataIn: Vec[ComplexFix] = Vec(gen.inputTypes.zip(dataIn).map { case (info, bits) =>
    val typed = info.asComplexFix()
    typed.assignFromBits(bits)
    typed
  })

  lazy val sfixDataIn: Vec[SFix] = Vec(gen.inputTypes.zip(dataIn).map { case (info, bits) =>
    val typed = info.asSFix()
    typed.assignFromBits(bits)
    typed
  })

  lazy val uintDataIn: Vec[UInt] = Vec(gen.inputTypes.zip(dataIn).map { case (info, bits) =>
    val typed = info.asUInt()
    typed.assignFromBits(bits)
    typed
  })

  lazy val complexDataOut: Vec[ComplexFix] = Vec(gen.outputTypes.zip(dataOut).map { case (info, bits) =>
    val typed = info.asComplexFix()
    bits := typed.asBits
    typed
  })

  lazy val sfixDataOut: Vec[SFix] = Vec(gen.outputTypes.zip(dataOut).map { case (info, bits) =>
    val typed = info.asSFix()
    bits := typed.asBits
    typed
  })

  lazy val uintDataOut: Vec[UInt] = Vec(gen.outputTypes.zip(dataOut).map { case (info, bits) =>
    val typed = info.asUInt()
    bits := typed.asBits
    typed
  })

//  lazy val realDataIn: Vec[AFix] = Vec(gen.outputTypes.zip(dataOut).map { case (info, bits) =>
//    val typed = info.asReal
//    bits := typed.asBits
//    typed
//  })
//
//  lazy val realDataOut: Vec[AFix] = Vec(gen.inputTypes.zip(dataIn).map { case (info, bits) =>
//    val typed = info.asReal
//    typed.assignFromBits(bits)
//    typed
//  })
//
//  lazy val complexDataIn: Vec[ComplexFixNew] = Vec(gen.outputTypes.zip(dataOut).map { case (info, bits) =>
//    val typed = info.asComplex
//    bits := typed.asBits
//    typed
//  })
//
//  lazy val complexDataOut: Vec[ComplexFixNew] = Vec(gen.inputTypes.zip(dataIn).map { case (info, bits) =>
//    val typed = info.asComplex
//    typed.assignFromBits(bits)
//    typed
//  })

  /** --------
   * connection utils
   * -------- */

  def >>(that: ChainsawModule): Unit = {
    require(this.gen.outputFormat == that.gen.inputFormat)
    that.flowIn := this.flowOut
  }

  def <<(that: ChainsawModule): Unit = that >> this

  def setFreeRun(): Unit = {
    this.flowIn.valid := True
    this.flowIn.last := True
  }
}