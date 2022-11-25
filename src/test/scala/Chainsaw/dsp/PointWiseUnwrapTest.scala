package Chainsaw.dsp

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

class PointWiseUnwrapTest extends ChainsawFlatSpec {

  val dataType = SFixInfo(5, 8)
  val gen = PointWiseUnwrap(dataType)
  testGenerator(gen)

}
