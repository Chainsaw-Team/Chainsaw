package Chainsaw.phases


import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._
import spinal.lib.fsm._
import spinal.lib.bus._
import spinal.core.internals._


object Classify{
  def apply(expression: Expression) = {
    expression match {
      case analogDriver: AnalogDriver => analogDriver match {
        case vector: AnalogDriverBitVector => ???
        case bool: AnalogDriverBool => ???
        case enum: AnalogDriverEnum => ???
      }
    }
  }
}
