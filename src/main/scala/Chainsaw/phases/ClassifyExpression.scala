package Chainsaw.phases

import spinal.core.internals._

object Classify {
  def apply(expression: Expression) = {
    expression match {
      case analogDriver: AnalogDriver =>
        analogDriver match {
          case vector: AnalogDriverBitVector => ???
          case bool: AnalogDriverBool        => ???
          case enum: AnalogDriverEnum        => ???
        }
    }
  }
}
