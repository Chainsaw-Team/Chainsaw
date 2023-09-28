package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.{UltraScale, XilinxDeviceFamily}
import Chainsaw.xilinx.XilinxDevice
import Chainsaw.{FLOPOCO, doCmd, pow2}
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class FloatingConstMult (
   exponentSize: Int,
   mantissaSize: Int,
   constant: Double,
   family: XilinxDeviceFamily,
   targetFrequency: HertzNumber
) extends FloatingOperator("FPConstMult", "FPConstMult",
  Seq(
    ("wE_in", exponentSize),
    ("wF_in", mantissaSize),
    ("wE_out", exponentSize),
    ("wF_out", mantissaSize),
    ("constant", constant)
  ), true, 1)

object SinglePrecisionFPConstMult {
  def apply(constant: Double, family: XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingConstMult(8, 23, constant, family, targetFrequency)
}

object DoublePrecisionFPConstMult {
  def apply(constant: Double, family: XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingConstMult(11, 52, constant, family, targetFrequency)
}

