package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.XilinxDeviceFamily
import spinal.core.HertzNumber

case class FloatingAdd(
                        exponentSize: Int,
                        mantissaSize: Int,
                        family: XilinxDeviceFamily,
                        targetFrequency: HertzNumber
                      ) extends FloatingOperator("FPAdd", "FPAdd", Seq(("wE", exponentSize), ("wF", mantissaSize), ("sub", false)), true, 2)

object SinglePrecisionFPAdd {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingAdd(8, 23, family, targetFrequency)
}

object DoublePrecisionFPAdd {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingAdd(11, 52, family, targetFrequency)
}
