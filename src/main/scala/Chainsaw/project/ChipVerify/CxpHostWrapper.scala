package Chainsaw.project.ChipVerify

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._
import spinal.lib.com.uart.Uart

case class CxpHostWrapper() extends BlackBox {

  setDefinitionName("cxp_host_wrapper")
  // ports(on board)
  val UART                         = master(Uart())
  val fmc_ref_clk_n, fmc_ref_clk_p = in Bool ()
  val fmc_rx_n, fmc_rx_p           = in Bits (4 bits)
  val fmc_tx                       = out Bits (4 bits)
  val pocxp_en_tri_o               = out Bits (4 bits)
  val power_good                   = out Bool ()
  // nets
  val dma_clk = out Bool ()
  val dmaOut  = master(CxpVideoDma(1, 4))
  dmaOut.setName("dma")

}
