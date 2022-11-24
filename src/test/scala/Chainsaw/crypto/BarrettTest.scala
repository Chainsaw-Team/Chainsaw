package Chainsaw.crypto

import Chainsaw._
import Chainsaw.testConfigurations._

class BarrettTest extends ChainsawFlatSpec {

  val width = 64
  val M = (BigInt(1) << 63) + 1
  val constants = Seq(Some(M)) // FIXME: implement the mode using variable modulus
  val multTypes = Seq(FullMultiplier, SquareMultiplier)

  constants.foreach(constant =>
    multTypes.foreach(multType =>
      testGenerator(Barrett(64, constant, multType), barrettSynth, barrettImpl)
    ))
}
