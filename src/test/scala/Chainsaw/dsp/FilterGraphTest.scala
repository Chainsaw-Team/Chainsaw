package Chainsaw.dsp

import Chainsaw.{ChainsawSynth, SFixInfo, matlabEngine}
import org.scalatest.flatspec.AnyFlatSpec

import Chainsaw._
import Chainsaw.xilinx._

class FilterGraphTest extends ChainsawFlatSpec {

  matlabEngine.eval("[b,a] = butter(2, 30e6/(250e6/2), 'low')")
  val a = matlabEngine.getVariable("a").asInstanceOf[Array[Double]]
  val b = matlabEngine.getVariable("b").asInstanceOf[Array[Double]]
  val filterPrecision = FilterPrecision(coeffType = SFixInfo(4, 12), dataType = SFixInfo(4, 12))

  val parallels = Seq(1, 2, 4)
  parallels.foreach(parallel => testGenerator(Filter(b, a, filterPrecision, parallel)))
}
