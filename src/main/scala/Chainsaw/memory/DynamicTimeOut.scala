package Chainsaw.memory

import spinal.core._

// TODO: verification for dynamic timeout
case class DynamicTimeOut(time: UInt) extends ImplicitArea[Bool] {

  val timeLocked = Reg(cloneOf(time))
  val valueNext  = cloneOf(time)
  val value      = RegNext(valueNext) init valueNext.getZero
  val state      = RegInit(False)

  when(value =/= U(0))(valueNext := value + 1)
    .otherwise(valueNext := U(0))

  when(valueNext === timeLocked)(state := True)

  def clear(): Unit = {
    timeLocked := time
    value      := U(1)
    state      := False
  }

  override def implicitValue: Bool = state
}
