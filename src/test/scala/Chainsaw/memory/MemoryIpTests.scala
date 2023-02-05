package Chainsaw.memory

import Chainsaw.{ChainsawFlatSpec, NumericType, TestConfig}

class MemoryIpTests extends ChainsawFlatSpec {

  def testP2S(): Unit = {
    testOperator(P2S(p = 8, s = 4, bitWidth = 16), generatorConfigTable("P2S"))
  }

  def testHeaderInserter(): Unit = {
    testOperator(
      FlowHeaderInserter(Seq(BigInt("ffeeddcc", 16)), 32),
      generatorConfigTable("FlowHeaderInserter")
    )
    testOperator(
      FlowHeaderInserter(
        Seq(BigInt("ffeeddcc", 16), BigInt("00112233", 16)),
        32
      ),
      generatorConfigTable("FlowHeaderInserter")
    )
  }

  def testWidthConverter(): Unit = {
    testOperator(
      FlowWidthConverter(16, 14, 2),
      generatorConfigTable("FlowWidthConverter")
    )
    testOperator(
      FlowWidthConverter(14, 16, 2),
      generatorConfigTable("FlowWidthConverter")
    )
  }

  def testDynamicDownSample(): Unit = {
    testOperator(
      DynamicDownSample(100, NumericType.U(16)),
      generatorConfigTable("DynamicDownSample")
    )
  }

  override def generatorConfigTable = Map(
    "P2S" -> TestConfig(full = true, naive = false, synth = true, impl = false),
    "FlowHeaderInserter" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    ),
    "FlowWidthConverter" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    ),
    "DynamicDownSample" -> TestConfig(
      full  = true,
      naive = false,
      synth = true,
      impl  = false
    )
  )

  testP2S()
  testHeaderInserter()
  testWidthConverter()
  testDynamicDownSample()
}
