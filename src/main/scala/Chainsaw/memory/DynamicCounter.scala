package Chainsaw.memory

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

// TODO: verification for dynamic counter
case class DynamicCounter(end: UInt) extends ImplicitArea[UInt] {

  val willIncrement = False.allowOverride
  val willClear = False.allowOverride

  def clear(): Unit = willClear := True

  def increment(): Unit = willIncrement := True

  def set(value: UInt): Unit = valueNext := value

  val width = end.getBitsWidth

  val valueNext = cloneOf(end)
  val value = RegNext(valueNext) init valueNext.getZero
  val willOverflowIfInc = value === end - U(1)
  val willOverflow = willOverflowIfInc && willIncrement

  when(willOverflow) {
    valueNext := U(0)
  } otherwise {
    valueNext := (value + U(willIncrement)).resized
  }
  when(willClear) {
    valueNext := 0
  }

  willOverflowIfInc.allowPruning()
  willOverflow.allowPruning()

  override def implicitValue = value
}


object DynamicCounter {
  def apply(end: UInt, inc: Bool): DynamicCounter = {
    val ret = DynamicCounter(end)
    when(inc)(ret.increment())
    ret
  }
}