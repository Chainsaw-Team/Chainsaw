package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.Device._
import spinal.core.{BlackBox, HertzNumber, IntToBuilder, in, out}

case class Flopoco2Ieee(
    exponentSize: Int,
    mantissaSize: Int,
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber
) extends BlackBox
    with floatingFlopoco {
  override val operatorName = "OutputIEEE"
  override val entityName   = "OutputIEEE"
  override val params =
    Seq(("wEIn", exponentSize), ("wFIn", mantissaSize), ("wEOut", exponentSize), ("wFOut", mantissaSize))

  val clk = in Bool ()
  mapClockDomain(clock = clk)
  val X = in Bits (exponentSize + mantissaSize + 1 + 2 bits)
  val R = out Bits (exponentSize + mantissaSize + 1 bits)
  addRTLPath(this.verilogFile.getAbsolutePath)
  setDefinitionName(this.moduleName)
}
