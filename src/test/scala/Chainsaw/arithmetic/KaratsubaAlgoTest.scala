package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class KaratsubaAlgoTest extends AnyFlatSpec {

  behavior of "karatsuba algo implementation"
  testKaraAlgo(96)
  testKaraAlgo(256)
  testKaraAlgo(377)
  testKaraAlgo(1024)
  behavior of "karatsuba algo search"
  testKaraSearch(1024)

  def testKaraAlgo(width: Int): Unit = {
    val data = Seq.fill(1000)(BigInt(width, Random), BigInt(width, Random))
    val algo = KaratsubaAlgo(width, RatioFirst, 500)
    it should s"work on $width bit multiplication" in {
      data.foreach { case (x, y) => algo.impl(x, y) }
    }
  }

  def testKaraSearch(width: Int) = {
    it should s"work on $width bit multiplication" in {
      val solution0 = KaratsubaAlgo(width, DspFirst, 1.0).solution
      val solution1 = KaratsubaAlgo(width, RatioFirst, 200.0).solution
      val solution2 = KaratsubaAlgo(width, ClbFirst, 1.0).solution
      assert(solution0.dspCost <= solution1.dspCost)
      assert(solution1.dspCost <= solution2.dspCost)
      assert(solution0.clbCost >= solution1.clbCost)
      assert(solution1.clbCost >= solution2.clbCost)
    }
  }


}
