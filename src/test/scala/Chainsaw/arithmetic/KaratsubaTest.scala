package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random

class KaratsubaTest extends AnyFlatSpec {

  behavior of "Karatsuba hardware"

  //  testFunction(30)
  // TODO: improve bitHeap performance on small input

  testFunction(64)
  //  testFunction(96)
  testFunction(377)
  //  testFunction(512)
  //  testImplementation(96)
  //  testImplementation(377)
  //  testImplementation(512)

  //  testFunction(1024)
  //  testImplementation(1024)

  def testFunction(width: Int): Unit = {
    val data = Seq.fill(1000)(BigInt(width, Random))
    //    val data = Seq.fill(1000)(BigInt(1))
    //    val data = Seq.fill(1000)(BigInt(0))
    it should s"work at width $width" in ChainsawTest(s"testKara$width", Karatsuba(width, None, DspFirst), data).doTest()
  }


  def testImplementation(width: Int): Unit = {

    it should s"synth at width $width" in ChainsawSynth(Karatsuba(width, None, DspFirst), s"synthKara$width")
  }


}
