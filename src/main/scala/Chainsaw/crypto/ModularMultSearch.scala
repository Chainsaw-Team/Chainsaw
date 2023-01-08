//package Chainsaw.crypto
//
//import Chainsaw._
//import Chainsaw.arithmetic._
//import Chainsaw.deprecated.{LsbBcm, MsbBcm}
//import Chainsaw.xilinx.VivadoUtil
//
//object ModularMultSearch {
//
//  def apply(width: Int, constantModulus: Option[BigInt] = None, multiplierType: MultiplierType, budget: VivadoUtil) = {
//
//    constantModulus match {
//      case Some(modulus) =>
//        val k = width
//        val algo = BarrettFineAlgo(modulus)
//        val msbWidthInvolved = algo.multMsb.widthInvolved
//        val lsbWidthInvolved = algo.multLsb.widthInvolved
//        val multSolutions = MultSearch.getBmParetos(width, multiplierType)
//        val multMsb = MsbBcm(algo.MPrime, widthIn = k + 1, widthInvolved = msbWidthInvolved, widthOut = k + 1, useCsd = true)
//        val multLsb = LsbBcm(modulus, widthIn = k + 1, widthOut = lsbWidthInvolved, useCsd = true)
//        logger.info(s"bcm part estimation = ${multMsb.utilEstimation + multLsb.utilEstimation}")
//        logger.warn(s"${multLsb.compressorGen.bitsCount}")
//        val utils = multSolutions.map(_.vivadoUtilEstimation + multMsb.utilEstimation + multLsb.utilEstimation)
//        budget.solveBestScheme(utils, Seq(0, 2))
//      case None => ???
//    }
//  }
//}
