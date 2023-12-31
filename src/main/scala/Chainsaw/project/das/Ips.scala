package Chainsaw.project.das

import spinal.core.{BlackBox, in, out}

case class LVDSDEBUG() extends BlackBox {
  val adc_clk, rstn = in Bool ()
  val lvds_clk      = out Bool ()
  val adcBundle     = in(Adc250())
  val adcData       = out(Adc62_5())
}
