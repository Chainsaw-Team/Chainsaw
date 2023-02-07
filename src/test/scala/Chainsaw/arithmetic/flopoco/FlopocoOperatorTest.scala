package Chainsaw.arithmetic.flopoco

import Chainsaw._

class FlopocoOperatorTest extends ChainsawFlatSpec {

  /** -------- IntMultiAdder
    * --------
    */
  def testIntMultiAdder(): Unit = {
    val operandWidths = Seq(10, 20, 30)
    val operandCounts = Seq(10)
    val signeds       = Seq(false, true)

    signeds.foreach(signed =>
      operandCounts.foreach(n =>
        operandWidths.foreach(widthIn =>
          testFlopocoOperator(
            IntMultiAdder(widthIn, n, signed),
            synth = false,
            impl  = false
          )
        )
      )
    )
  }

  /** -------- IntMultiplier
    * --------
    */
  def testIntMultiplier(): Unit = {
    val multWidths = Seq(24)
    multWidths.foreach(width =>
      testFlopocoOperator(
        IntMultiplier(width, width, 1),
        synth = false,
        impl  = false
      )
    )
  }

  /** -------- BaseKaratsuba
    * --------
    */
  def testBaseKaratsuba(): Unit = {
    val ns = Seq(2, 3, 4)
    ns.foreach(n =>
      testFlopocoOperator(
        BaseMultiplierDSPKaratsuba(17, 17, n),
        synth = false,
        impl  = false
      )
    )
  }

  override val generatorConfigTable = Map(
    "IntMultiAdder" -> TestConfig(
      full  = false,
      naive = true,
      synth = false,
      impl  = false
    ),
    "IntMultiplier" -> TestConfig(
      full  = false,
      naive = true,
      synth = false,
      impl  = false
    ),
    "BaseKaratsuba" -> TestConfig(
      full  = false,
      naive = true,
      synth = false,
      impl  = false
    )
  )

  // TODO: check existence of flopoco
  if (hasFlopoco) {
    testIntMultiAdder()
    testIntMultiplier()
    testBaseKaratsuba()
  }
}
