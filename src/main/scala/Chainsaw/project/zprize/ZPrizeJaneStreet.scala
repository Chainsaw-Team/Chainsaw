package Chainsaw.project.zprize

import Chainsaw.edaFlow.vivado._

import java.io.File

object ZPrizeJaneStreet extends App {

  val krnlDir = new File("/home/ltr/Chainsaw/krnl_msm_pippenger")

  //  VivadoSynth(netlistDir = krnlDir, topModuleName = "kernel_for_vitis")
  VivadoTask.synthNetlist("karatsuba_ofman_stage_377_radix_2", Seq(krnlDir))
//  VivadoTask.synth(netlistDir = krnlDir, topModuleName = "barrett_reduction_377")

}
