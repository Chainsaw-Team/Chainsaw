package Chainsaw.dsp

import Chainsaw.{ChainsawSynth, SFixInfo, matlabEngine}
import org.scalatest.flatspec.AnyFlatSpec

import Chainsaw._
import Chainsaw.xilinx._

class FilterGraphTest extends AnyFlatSpec {

  behavior of "FilterGraph"

  it should "work for IIR filter" in {

    matlabEngine.eval("[b,a] = butter(2, 30e6/(250e6/2), 'low')")
    val a = matlabEngine.getVariable("a").asInstanceOf[Array[Double]]
    val b = matlabEngine.getVariable("b").asInstanceOf[Array[Double]]

    val filterPrecision = FilterPrecision(coeffType = SFixInfo(4, 12), dataType = SFixInfo(4, 12))
    val gen = SingleFilterGraph(b, a, filterPrecision)

    val data = Seq.fill(20)(1.0)
    ChainsawTest("testIIR", gen, data).doTest()
    ChainsawSynth(gen, "synthIIR")
    ChainsawTest("testIIR", Unfold(gen, 4), data).doTest()
    ChainsawSynth(Unfold(gen, 4), "synthIIR")

  }
}
