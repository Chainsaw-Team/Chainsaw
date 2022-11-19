package Chainsaw.dsp

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps

class DdsWaveTest extends AnyFlatSpec {

  behavior of "Dds"

  it should "work for sine wave" in
    ChainsawTest(
      testName = "testDds",
      gen = Dds(DdsWave(SINE, 250 MHz, 80 MHz, 0), dataType = SFixInfo(1, 16)),
      data = Seq.fill(1000)(BigInt(1)))
      .doTest()

}
