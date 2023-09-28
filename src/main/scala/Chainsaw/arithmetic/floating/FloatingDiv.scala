package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.{UltraScale, XilinxDeviceFamily}
import Chainsaw.xilinx.XilinxDevice
import Chainsaw.{FLOPOCO, doCmd, pow2}
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class FloatingDiv(
  exponentSize: Int,
  mantissaSize: Int,
  family: XilinxDeviceFamily,
  targetFrequency: HertzNumber
) extends FloatingOperator("FPDiv", "FPDiv", Seq(("wE", exponentSize), ("wF", mantissaSize)), true, 2)

object SinglePrecisionFPDiv {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingDiv(8, 23, family, targetFrequency)
}

object DoublePrecisionFPDiv {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingDiv(11, 52, family, targetFrequency)
}


