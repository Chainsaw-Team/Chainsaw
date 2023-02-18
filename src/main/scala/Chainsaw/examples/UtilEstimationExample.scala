package Chainsaw.examples

import Chainsaw._
import Chainsaw.dsp._
import Chainsaw.xilinx._ // for Xilinx FPGA Flow
class UtilEstimationExample extends App {
  val parameters = Seq(8, 12, 16)
  val utils: Seq[(Int, Int, VivadoUtil)] = Seq
    .tabulate(3, 3) { (iter, frac) =>
      val iteration  = parameters(iter)
      val fractional = parameters(frac)
      val report     = ChainsawSynth(Cordic(CIRCULAR, ROTATION, iteration, fractional))
      (iteration, fractional, report.util)
    }
    .flatten
}
