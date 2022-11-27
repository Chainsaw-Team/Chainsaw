package Chainsaw.dsp

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

class PointwiseUnwrapTest extends ChainsawFlatSpec {

  val dataType = SFixInfo(5, 16)
  val gen = PointwiseUnwrap(dataType)
  testGenerator(gen)

}
