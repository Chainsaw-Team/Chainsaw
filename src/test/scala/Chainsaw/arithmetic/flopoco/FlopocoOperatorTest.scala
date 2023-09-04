package Chainsaw.arithmetic.flopoco

import Chainsaw._
import Chainsaw.edaFlow.UltraScale
import spinal.core.IntToBuilder

import scala.language.postfixOps

class FlopocoOperatorTest extends ChainsawFlatSpec {

  def testIntMultiAdder(): Unit = {
    val operandWidths = Seq(10, 20, 30)
    val operandCounts = Seq(10)
    val signeds = Seq(false, true)

    signeds.foreach(signed =>
      operandCounts.foreach(n =>
        operandWidths.foreach(widthIn =>
          testOperator(IntMultiAdder(UltraScale, 400 MHz, widthIn, n, signed), generatorConfigTable("IntMultiAdder")))
      )
    )
  }

  def testIntMultiplier(): Unit = {
    val multWidths = Seq(24, 32)
    multWidths.foreach(width => testOperator(IntMultiplier(UltraScale, 400 MHz, width, width, 1), generatorConfigTable("IntMultiplier")))
  }

  def testBaseKaratsuba(): Unit = {
    val ns = Seq(2, 3, 4)
    ns.foreach(n => testOperator(BaseMultiplierDSPKaratsuba(UltraScale, 400 MHz, 17, 17, n), generatorConfigTable("BaseKaratsuba")))
  }

  override def generatorConfigTable: Map[String, TestConfig] = Map(
    "IntMultiAdder" -> TestConfig(
      full = true,
      naive = false,
      synth = true,
      impl = false
    ),
    "IntMultiplier" -> TestConfig(
      full = true,
      naive = false,
      synth = true,
      impl = false
    ),
    "BaseKaratsuba" -> TestConfig(
      full = true,
      naive = false,
      synth = true,
      impl = false
    )
  )

  if (FLOPOCO.exist()) {
    testIntMultiAdder()
    testIntMultiplier()
    testBaseKaratsuba()
  }

}
