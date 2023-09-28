package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.{UltraScale, XilinxDeviceFamily}
import Chainsaw.xilinx.XilinxDevice
import Chainsaw.{FLOPOCO, doCmd, pow2}
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class FloatingSub(
  exponentSize: Int,
  mantissaSize: Int,
  family: XilinxDeviceFamily,
  targetFrequency: HertzNumber
) extends FloatingOperator("FPAdd", "FPSub", Seq(("wE", exponentSize), ("wF", mantissaSize), ("sub", true)), true, 2)

object SinglePrecisionFPSub {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingSub(8, 23, family, targetFrequency)
}

object DoublePrecisionFPSub {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingSub(11, 52, family, targetFrequency)
}

