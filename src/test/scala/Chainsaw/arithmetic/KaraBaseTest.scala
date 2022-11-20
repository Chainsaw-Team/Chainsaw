package Chainsaw.arithmetic

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class KaraBaseTest extends AnyFlatSpec {

  def testKaraBase(mode: MultiplierType) = {
//    val widthA = 8
//    val widthB = if (mode == Kara) 10 else 8
    val widthA = 16
    val widthB = if (mode == Kara) 25 else 16
    val data = Seq.fill(1000)(Seq(BigInt(widthA, Random), BigInt(widthA, Random), BigInt(widthB, Random), BigInt(widthB, Random))).flatten
    val gen = KaraBase(widthA, widthB, mode)
    ChainsawTest("testKaraBase", gen, data).doTest()
  }

  val modes = Seq(FullMultiplier, SquareMultiplier, MsbMultiplier, LsbMultiplier, Kara)

  behavior of "kara base"

  modes.foreach(mode => it should s"work for ${mode.getClass.getSimpleName} mode" in testKaraBase(mode))

  modes.foreach(mode => it should s"synth for ${mode.getClass.getSimpleName} mode" in ChainsawSynth(KaraBase(16, 16, mode), "synthKaraBase", withRequirement = true))

  modes.foreach(mode => it should s"Impl for ${mode.getClass.getSimpleName} mode" in ChainsawImpl(KaraBase(16, 16, mode), "implKaraBase", withRequirement = true))

}