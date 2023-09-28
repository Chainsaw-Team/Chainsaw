package Chainsaw.arithmetic.flopoco


import Chainsaw._
import Chainsaw.edaFlow.UltraScale
import spinal.core.IntToBuilder
import spinal.lib.cpu.riscv.impl.Utils.M
import java.lang._
import scala._
import Predef._

import scala.language.postfixOps

class FlopocoOperatorTest extends ChainsawFlatSpec {

  def testGenericMux(): Unit = {
    val wIn = Seq(12, 16, 24)
    val inputCount = Seq(8, 12, 16, 24)
    val fre = Seq(200, 400)
    wIn.foreach(wIn=>
      inputCount.foreach(inputCount=>
        fre.foreach(fre=>
          testOperator(GenericMux(UltraScale, fre MHz, wIn, inputCount), generatorConfigTable("GenericMux"))
        )
      )
    )
  }

  def testGenericLut(): Unit = {
    val outputValues= Seq(7, 2, 4, 12, 4, 5)
    val wIn= 3
    val wOut = 4
    val lutName = "LUTtest"  // this should be unique
    val fre = Seq(200, 400)
    fre.foreach(fre=>
      testOperator(GenericLut(UltraScale, fre MHz, wIn, wOut, outputValues, lutName), generatorConfigTable("GenericLut"))
    )
  }

  def testIntMultiAdder(): Unit = {
    val operandWidths = Seq(10, 20, 30)
    val operandCounts = Seq(10)
    val signeds = Seq(false, true)
    val fre = Seq(200, 400)

    signeds.foreach(signed =>
      operandCounts.foreach(n =>
        operandWidths.foreach(widthIn =>
          fre.foreach(fre=>
            testOperator(IntMultiAdder(UltraScale, fre MHz, widthIn, n, signed), generatorConfigTable("IntMultiAdder"))
          )
        )
      )
    )
  }

  def testIntMultiplier(): Unit = {
    val multWidths = Seq(24, 32)
    val fre = Seq(200, 400)

    fre.foreach(fre=>
      multWidths.foreach(width => testOperator(IntMultiplier(UltraScale, fre MHz, width, width, 1), generatorConfigTable("IntMultiplier")))
    )
  }

  def testBaseKaratsuba(): Unit = {
    val ns = Seq(2, 3, 4)
    val fre = Seq(200, 400)

    fre.foreach(fre=>
      ns.foreach(n => testOperator(BaseMultiplierDSPKaratsuba(UltraScale, fre MHz, 17, 17, n), generatorConfigTable("BaseKaratsuba")))
    )
  }

  def testIntAdder(): Unit = {
    val width = Seq(5,6,7)
    val fre = Seq(200, 400)

    width.foreach(wIn =>
      fre.foreach(fre =>
        testOperator(IntAdder(UltraScale, fre MHz, wIn), generatorConfigTable("IntAdder"))
      )
    )
  }

  def testIntDualAddSub(): Unit = {
    val width = Seq(5,6,7)
    val fre = Seq(200, 400)

    width.foreach(wIn =>
      fre.foreach(fre=>
        testOperator(IntDualAddSub(UltraScale, fre MHz, wIn), generatorConfigTable("IntDualAddSub"))
      )
    )
  }

  def testIntSquarer(): Unit = {
    val width = Seq(8,16,24)
    val fre = Seq(200, 400)

    width.foreach(wIn =>
      fre.foreach(fre=>
        testOperator(IntSquarer(UltraScale, fre MHz, wIn), generatorConfigTable("IntSquarer"))
      )

    )
  }

  def testIntConstMult(): Unit = {
    val width = Seq(8, 16)
    val const = Seq(12, 20)
    val fre = Seq(200, 400)

    width.foreach(width =>
      const.foreach(const=>
        fre.foreach(fre=>
          testOperator(IntConstMult(UltraScale, fre MHz, width, const), generatorConfigTable("IntConstMult"))
        )
      )
    )
  }

  def testIntConstDiv(): Unit = {
    val width = Seq(8, 16)
    val divisor = Seq(3, 5, 7)
    val arch = Seq(0, 1, 2)
    val fre = Seq(200, 400)

    width.foreach(width =>
      divisor.foreach(divisor=>
        arch.foreach(arch=>
          fre.foreach(fre=>
            testOperator(IntConstDiv(UltraScale, fre MHz, width, divisor, true, true, arch), generatorConfigTable("IntConstDiv"))
          )
        )
      )
    )
  }

  def testDSPBlock(): Unit = {
    val wX = Seq(8, 16)
    val wY = Seq(8, 16)
    val wZ = Seq(8, 16)
    val xIsSigned = Seq(false, true)
    val yIsSigned = Seq(false, true)
    val isPipelined = Seq(false, true)
    val usePostAdder = Seq(false, true)
    val usePreAdder = Seq(false, true)
    val usePreSubtractor = Seq(false, true)
    val fre = Seq(200, 400)

    wX.foreach(wX=>
      wY.foreach(wY=>
        wZ.foreach(wZ=>
          xIsSigned.foreach(xIsSigned=>
            yIsSigned.foreach(yIsSigned=>
              isPipelined.foreach(isPipelined=>
                usePostAdder.foreach(usePostAdder=>
                  usePreAdder.foreach(usePreAdder=>
                    usePreSubtractor.foreach(usePreSubtractor=>
                      fre.foreach(fre=>
                        testOperator(DSPBlock(UltraScale, fre MHz, wX, wY, wZ, xIsSigned, yIsSigned, isPipelined, usePostAdder, usePreAdder, usePreSubtractor), generatorConfigTable("DSPBlock"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def testIntMultiplierLUT(): Unit = {
    val wX = Seq(4, 5)
    val wY = Seq(4, 5)
    val fre = Seq(200, 400)

    wX.foreach(wX =>
      wY.foreach(wY =>
        fre.foreach(fre=>
          testOperator(IntMultiplierLUT(UltraScale, fre MHz, wX, wY), generatorConfigTable("IntMultiplierLUT"))
        )
      )
    )
  }

  def testIntKaratsubaRectangular(): Unit = {
    val fre = Seq(200, 400)
    fre.foreach(fre=>
      testOperator(IntKaratsubaRectangular(UltraScale, fre MHz, 17, 17, 0, 0), generatorConfigTable("IntKaratsubaRectangular"))
    )
  }

  def testFixSinCos(): Unit = {
    val lsb = Seq(-8, -10, -12)
    val method = Seq(0, 1, 2)
    val fre = Seq(200, 400)

    lsb.foreach(lsb =>
      method.foreach(method=>
        fre.foreach(fre=>
          testOperator(FixSinCos(UltraScale, fre MHz, lsb, method), generatorConfigTable("FixSinCos"))
        )

      )
    )
  }


  def testLZOC(): Unit = {
    val wIn = Seq(16, 24, 32)
    val countType = Seq(-1, 0, 1)
    val fre = Seq(200, 400)

    wIn.foreach(wIn=>
      countType.foreach(countType=>
        fre.foreach(fre=>
          testOperator(LZOC(UltraScale, fre MHz, wIn, countType), generatorConfigTable("LZOC"))
        )
      )
    )
  }

  def testLZOC3(): Unit = {
    val wIn = Seq(24, 16, 32)
    val useLargeLut = Seq(false, true)
    val fre = Seq(200, 400)

    wIn.foreach(wIn =>
      useLargeLut.foreach(useLargeLut =>
        fre.foreach(fre=>
          testOperator(LZOC3(UltraScale, fre MHz, wIn, useLargeLut), generatorConfigTable("LZOC3"))
        )
      )
    )
  }


  def testFixSOPC(): Unit = {
    val lsbIn = Seq(-12, -16)
    val lsbOut = Seq(-12, -16)
    val constant = Seq(Math.PI, 1.5, 0.3)
    val fre = Seq(200, 400)

    lsbIn.foreach(lsbIn =>
      lsbOut.foreach(lsbOut =>
        constant.foreach(constant=>
          fre.foreach(fre=>
            testOperator(FixSOPC(UltraScale, fre MHz, lsbIn, lsbOut, constant), generatorConfigTable("FixSOPC"))
          )
        )
      )
    )
  }

  def testFixRealKCM(): Unit = {
    val signedIn = Seq(false, true)
    val integralIn = Seq(6, 8)
    val fractionalIn = Seq(12, 16)
    val fractionalOut = Seq(12, 16)
    val constant = Seq(0.11111, 0.777)
    val fre = Seq(200, 400)

    signedIn.foreach(signedIn=>
      integralIn.foreach(integralIn=>
        fractionalIn.foreach(fractionalIn=>
          fractionalOut.foreach(fractionalOut=>
            constant.foreach(constant=>
              fre.foreach(fre=>
                testOperator(FixRealKCM(UltraScale, fre MHz, signedIn, integralIn, fractionalIn, fractionalOut, constant), generatorConfigTable("FixRealKCM"))
              )
            )
          )
        )
      )
    )
  }

  override def generatorConfigTable: Map[String, TestConfig] = Map(
    "IntMultiAdder" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),
    "IntMultiplier" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),
    "BaseKaratsuba" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "IntAdder" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "IntDualAddSub" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "IntSquarer" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "IntConstMult" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "IntConstDiv" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "DSPBlock" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "IntMultiplierLUT" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "IntKaratsubaRectangular" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "GenericMux" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "GenericLut" -> TestConfig(
      full = false,
      naive = false,
      synth = false,
      impl = false
    ),

    "FixSinCos" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "LZOC" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "LZOC3" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "FixRealKCM" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    ),

    "FixSOPC" -> TestConfig(
      full = true,
      naive = false,
      synth = false,
      impl = false
    )
  )

  if (FLOPOCO.exist()) {
    testIntMultiAdder()
    testIntMultiplier()
    testBaseKaratsuba()
    testIntAdder()
    testIntDualAddSub()
    testIntSquarer()
    testIntConstMult()
    testDSPBlock()
    testIntConstDiv()
    testIntMultiplierLUT()
    testIntKaratsubaRectangular()
    testGenericMux()
    testGenericLut()
    testFixSinCos()
    testLZOC()
    testLZOC3()
    testFixRealKCM()
    testFixSOPC()
  }

}
