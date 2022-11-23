package Chainsaw.arithmetic

import Chainsaw._

class DedicatedMultTest extends ChainsawFlatSpec {
  val modes = Seq(FullMultiplier, SquareMultiplier, MsbMultiplier, LsbMultiplier, Kara)
  modes.foreach { mode =>
    val widthA = 16
    val widthB = if (mode == Kara) 25 else 16
    val gen = DedicatedMult(widthA, widthB, mode)
    //    testGenerator(gen, synth = true, impl = true)
    testGenerator(gen)
  }
}