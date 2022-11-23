package Chainsaw.arithmetic

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class DedicatedMultTest extends AnyFlatSpec {

  def testMults(mode: MultiplierType): Unit = {
    val widthA = 16
    val widthB = if (mode == Kara) 25 else 16
    val gen = DedicatedMult(widthA, widthB, mode)
    it should s"work for ${mode.getClass.getSimpleName} mode" in gen.selfTest()
  }

  val modes = Seq(FullMultiplier, SquareMultiplier, MsbMultiplier, LsbMultiplier, Kara)

  behavior of "dedicated mults"

  //  naiveSet += "DedicatedMult"
  modes.foreach(testMults)
  modes.foreach(mode => it should s"synth for ${mode.getClass.getSimpleName} mode" in ChainsawSynth(DedicatedMult(16, 16, mode), "synthKaraBase", withRequirement = true))
  modes.foreach(mode => it should s"Impl for ${mode.getClass.getSimpleName} mode" in ChainsawImpl(DedicatedMult(16, 16, mode), "implKaraBase", withRequirement = true))

}