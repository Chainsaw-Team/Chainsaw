package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.{UltraScale, XilinxDeviceFamily}
import Chainsaw.xilinx.XilinxDevice
import Chainsaw.{FLOPOCO, doCmd, pow2}
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class FloatingAddWithoutConvert(
  exponentSize: Int,
  mantissaSize: Int,
  family: XilinxDeviceFamily,
  targetFrequency: HertzNumber
) extends FloatingOperator ("IEEEAdd", "IEEEAdd", Seq(("wE", exponentSize), ("wF", mantissaSize)), false, 2)

object SinglePrecisionFPAddWithoutConvert {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingAddWithoutConvert(8, 23, family, targetFrequency)
}

object DoublePrecisionFPAddWithoutConvert {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingAddWithoutConvert(11, 52, family, targetFrequency)
}

