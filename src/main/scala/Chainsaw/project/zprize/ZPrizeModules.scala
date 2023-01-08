package Chainsaw.project.zprize

import Chainsaw.crypto.BarrettFineAlgo
import Chainsaw.project.zprize.ZPrizeMSM.{MPrime, baseModulus}
import Chainsaw.{LsbMultiplier, MsbMultiplier}
import Chainsaw.arithmetic._

object ZPrizeModules {

  val dataWidth = baseModulus.bitLength
  val algo = BarrettFineAlgo(baseModulus)
  val msbWidthInvolved = algo.multMsb.widthInvolved
  val lsbWidthInvolved = algo.multLsb.widthInvolved

  def msbMultGen = Bcm(MPrime, MsbMultiplier, widthIn = dataWidth + 1, widthInvolved = msbWidthInvolved, widthOut = dataWidth + 1)

  def lsbMultGen = Bcm(baseModulus, LsbMultiplier, widthIn = dataWidth, widthInvolved = lsbWidthInvolved, widthOut = lsbWidthInvolved)
}
