package Chainsaw.project.zprize

import Chainsaw.project.zprize.ZPrizeModules.{lsbMultGen, msbMultGen}
import Chainsaw.{ChainsawSynth, logger}

object ZprizeSynthesis extends App {

  logger.info("BCMs synthesis")
  ChainsawSynth(msbMultGen)
  ChainsawSynth(lsbMultGen)

}
