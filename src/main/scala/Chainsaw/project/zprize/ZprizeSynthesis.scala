package Chainsaw.project.zprize

import Chainsaw.{ChainsawSynthOld, logger}
import Chainsaw.project.zprize.ZPrizeModules.{lsbMultGen, msbMultGen}

object ZprizeSynthesis extends App {

  logger.info("BCMs synthesis")
  ChainsawSynthOld(msbMultGen, "synth377Msb")
  ChainsawSynthOld(lsbMultGen, "synth377Lsb")

}
