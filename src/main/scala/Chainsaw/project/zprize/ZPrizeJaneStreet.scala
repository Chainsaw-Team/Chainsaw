package Chainsaw.project.zprize


import Chainsaw._
import Chainsaw.xilinx._

import java.io.File

object ZPrizeJaneStreet extends App {

  val krnlDir = new File("/home/ltr/Chainsaw/krnl_msm_pippenger")

  //  VivadoSynth(netlistDir = krnlDir, topModuleName = "kernel_for_vitis")
  VivadoSynth(netlistDir = krnlDir, topModuleName = "karatsuba_ofman_stage_377_radix_2")
  VivadoSynth(netlistDir = krnlDir, topModuleName = "barrett_reduction_377")

}
