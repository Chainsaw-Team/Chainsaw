package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class KaratsubaAlgoTest extends AnyFlatSpec {

  behavior of "karatsuba algo implementation"
  //  testKaraAlgo(96)
  //  testKaraAlgo(256)
  //  testKaraAlgo(377)
  //  testKaraAlgo(1024)
  testKaraAlgo(4096)

  behavior of "karatsuba search algo"
  //  testKaraSearchStrategy(377)
  //  testKaraSearchStrategy(1024)
  //  testKaraSearchLayer(4)
  //  testKaraSearchLayer(10)
  //  testKaraSearchAll()

  def testKaraAlgo(width: Int): Unit = {
    val data = Seq.fill(1000)(BigInt(width, Random), BigInt(width, Random))
    it should s"work on $width bit multiplication" in {
      val algo = KaratsubaAlgo(width, DspFirst)
      data.foreach { case (x, y) => algo.impl(x, y) }
    }
  }

  def testKaraSearchStrategy(width: Int): Unit = {
    it should s"work on $width bit multiplication" in {
      val solution0 = KaratsubaAlgo(width, DspFirst).solution
      val solution1 = KaratsubaAlgo(width, RatioFirst, 10, 50.0).solution
      val solution2 = KaratsubaAlgo(width, ClbFirst).solution
      assert(solution0.dspCost <= solution1.dspCost)
      assert(solution1.dspCost <= solution2.dspCost)
      assert(solution0.clbCost >= solution1.clbCost)
      assert(solution1.clbCost >= solution2.clbCost)
    }
  }

  def testKaraSearchLayer(layer: Int): Unit = {
    it should s"work on when layer is limited as $layer" in {
      KaratsubaAlgo(1024, DspFirst, layer)
    }
  }

  def testKaraSearchAll(): Unit = {
    it should "work for all widths" in {
      (16 to 1024 by 16).foreach(KaratsubaAlgo(_, DspFirst))
    }
  }
}
