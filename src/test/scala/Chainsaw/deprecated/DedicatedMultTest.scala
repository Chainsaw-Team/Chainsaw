package Chainsaw.deprecated

import Chainsaw._
import Chainsaw.testConfigurations._

class DedicatedMultTest extends ChainsawFlatSpec {
  val modes = Seq(FullMultiplier, SquareMultiplier, LsbMultiplier, Kara) // FIXME: implement MsbMultiplier, add it to modes
  modes.foreach { mode =>
    val widthA = 16
    val widthB = if (mode == Kara) 25 else 16
    val gen = DedicatedMult(widthA, widthB, mode)
    testGenerator(gen, synth = dedicatedMultSynth, impl = dedicatedMultImpl)
  }
}