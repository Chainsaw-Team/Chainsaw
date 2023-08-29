package Chainsaw

import Chainsaw.phases._
import Chainsaw.xilinx._
import spinal.core._

/** customized elaboration configuration for ChainsawGenerator
  */
object ChainsawSpinalConfig {
  def apply(gen: ChainsawBaseGenerator) = {
    val base = SpinalConfig(
      defaultConfigForClockDomains = xilinxCDConfig,
      targetDirectory              = "./tmpRtl/",
      oneFilePerComponent          = !atSimTime
    )
    if (gen.isInstanceOf[OverwriteLatency] && !gen.useNaive)
      base.addTransformationPhase(new Retiming)
    if (gen.isInstanceOf[Unaligned])
      base.addTransformationPhase(new IoAlign) // for unaligned generator, pad the input and output
    //    if (!atSimTime) base.addTransformationPhase(new phases.FfIo) // TODO: subtract the additional FFs from synth/impl result?
    if (verbose >= 1) base.addTransformationPhase(new AreaEstimation)
    base.addTransformationPhase(new DrawHierarchy)
    logger.info("add retiming")
    base
  }

  def apply() = {
    val base = SpinalConfig(
      defaultConfigForClockDomains = xilinxCDConfig,
      targetDirectory              = "./tmpRtl/",
      oneFilePerComponent          = true
    )
    base.addTransformationPhase(new phases.FfIo)
    base
  }
}
