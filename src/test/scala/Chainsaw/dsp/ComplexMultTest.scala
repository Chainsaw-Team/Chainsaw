package Chainsaw.dsp

import Chainsaw._

class ComplexMultTest extends ChainsawFlatSpec {

  val dataType = ComplexFixInfo(2, 14)
  val productType = ComplexFixInfo(5, 11)
  val gen = ComplexMult(dataType, dataType, productType)

  testGenerator(gen, synth = true)
}
