package Chainsaw.device

import Chainsaw.{ChainsawFlatSpec, TestConfig}
import spinal.core._

class DeviceIpTests extends ChainsawFlatSpec {

  def testLUT6_2(): Unit = {

    // test the factory method from logic expression to LUT6_2
    val exp0 = (i0: Boolean, i1: Boolean, i2: Boolean, i3: Boolean, i4: Boolean) =>
      (i0.toInt + i1.toInt + i2.toInt) >= 2

    val exp1 = (i0: Boolean, i1: Boolean, i2: Boolean, i3: Boolean, i4: Boolean) =>
      i0 ^ i1 ^ i2 ^ i3

    val gen = LUT5to2(exp0, exp1)
    testOperator(gen, generatorConfigTable("LUT6_2"))
  }

  override def generatorConfigTable = Map(
    "LUT6_2" -> TestConfig(full = true, naive = true, synth = true, impl = true)
  )

  testLUT6_2()
}
