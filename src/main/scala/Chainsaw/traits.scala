package Chainsaw

import Chainsaw.xilinx.{VivadoUtil, VivadoUtilEstimation}

trait HardAlgo {
  def vivadoUtilEstimation: VivadoUtil
}
