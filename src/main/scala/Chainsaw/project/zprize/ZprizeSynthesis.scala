package Chainsaw.project.zprize

import Chainsaw.{ChainsawSynth, logger}
import Chainsaw.project.zprize.ZPrizeModules.{lsbMultGen, msbMultGen}

object ZprizeSynthesis extends App {

  logger.info("BCMs synthesis")
  ChainsawSynth(msbMultGen)
  ChainsawSynth(lsbMultGen)

}
