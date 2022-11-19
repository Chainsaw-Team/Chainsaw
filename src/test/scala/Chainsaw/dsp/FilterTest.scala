package Chainsaw.dsp

import org.scalatest.flatspec.AnyFlatSpec

import Chainsaw._
import Chainsaw.xilinx._

class FilterTest extends AnyFlatSpec {

  behavior of "Filter"

  it should "work for IIR filter" in {

    matlabEngine.eval("[b,a] = butter(5, 30e6/(250e6/2), 'low')")
    val a = matlabEngine.getVariable("a").asInstanceOf[Array[Double]]
    val b = matlabEngine.getVariable("b").asInstanceOf[Array[Double]]

    val gen = Filter(b, a, coeffType = SFixInfo(4, 12), dataType = SFixInfo(4, 12))

    val data = Seq.fill(20)(1.0)
    //    ChainsawTest("testIIR", gen, data).doTest()
    ChainsawSynth(gen, "testIIR")
  }

}
