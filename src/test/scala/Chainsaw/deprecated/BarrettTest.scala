package Chainsaw.deprecated

import Chainsaw.testConfigurations._
import Chainsaw.{deprecated, _}

class BarrettTest extends ChainsawFlatSpec {

  val width = 64
  val M = (BigInt(1) << 63) + 1
  val constants = Seq(Some(M)) // FIXME: implement the mode using variable modulus
  val multTypes = Seq(FullMultiplier, SquareMultiplier)

  constants.foreach(constant =>
    multTypes.foreach { multType =>
      logger.warn(s"${Barrett(64, constant, multType).latency}")
      testGenerator(deprecated.Barrett(64, constant, multType), barrettSynth, barrettImpl)
    })
}
