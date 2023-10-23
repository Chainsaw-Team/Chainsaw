package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.{UltraScale, XilinxDeviceFamily}
import Chainsaw.xilinx.XilinxDevice
import Chainsaw.{FLOPOCO, doCmd, pow2}
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class FloatingMult(
   exponentSize: Int,
   mantissaSize: Int,
   family: XilinxDeviceFamily,
   targetFrequency: HertzNumber
) extends FloatingOperator("FPMult", "FPMult", Seq(("wE", exponentSize), ("wF", mantissaSize)), true, 2)

object SinglePrecisionFPMult {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingMult(8, 23, family, targetFrequency)
}

object DoublePrecisionFPMult {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingMult(11, 52, family, targetFrequency)
}
