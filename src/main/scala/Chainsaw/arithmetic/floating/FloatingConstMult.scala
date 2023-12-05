package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.Device._
import Chainsaw.{FLOPOCO, doCmd, pow2}
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class FlopocoConstMult(
    exponentSizeIn: Int,
    mantissaSizeIn: Int,
    exponentSizeOut: Int,
    mantissaSizeOut: Int,
    constant: Double,
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber
) extends BlackBox
    with floatingFlopoco {
  override val operatorName = "FPConstMult"
  override val entityName   = operatorName
  override val params = Seq(
    ("wE_in", exponentSizeIn),
    ("wF_in", mantissaSizeIn),
    ("wE_out", exponentSizeOut),
    ("wF_out", mantissaSizeOut),
    ("constant", "\"" + constant + "\"")
  )

  val clk = in Bool ()
  mapClockDomain(clock = clk)
  val X = in Bits (exponentSizeIn + mantissaSizeIn + 2 bits)
  val R = out Bits (exponentSizeOut + mantissaSizeOut + 2 bits)

  addRTLPath(this.verilogFile.getAbsolutePath)
  setDefinitionName(this.moduleName)
}

case class FloatingConstMult(
    exponentSizeIn: Int,
    mantissaSizeIn: Int,
    exponentSizeOut: Int,
    mantissaSizeOut: Int,
    constant: Double,
    family: XilinxDeviceFamily,
    targetFrequency: HertzNumber
) extends Module {
  val x = in(Floating(exponentSizeIn, mantissaSizeIn))
  val z = out(Floating(exponentSizeOut, mantissaSizeOut))

  Seq(x, z).foreach(_.flattenForeach(_.simPublic()))

  val i2f = Ieee2Flopoco(exponentSizeIn, mantissaSizeIn, family, targetFrequency)
  val f2i = Flopoco2Ieee(exponentSizeOut, mantissaSizeOut, family, targetFrequency)
  val constMult = FlopocoConstMult(
    exponentSizeIn,
    mantissaSizeIn,
    exponentSizeOut,
    mantissaSizeOut,
    constant,
    family,
    targetFrequency
  )

  i2f.X       := x.asBits
  constMult.X := i2f.R(i2f.R.high - 1, 2 bits) ## i2f.R(0, (exponentSizeIn + mantissaSizeIn) bits)
  f2i.X := constMult.R(constMult.R.high - 1, 2 bits) ## False.asBits ## constMult.R(
    0,
    (exponentSizeOut + mantissaSizeOut) bits
  )
  z.assignFromBits(RegNext(f2i.R)) // latency = 1
}

object SinglePrecisionFPConstMult {
  def apply(constant: Double, family: XilinxDeviceFamily, targetFrequency: HertzNumber) =
    FloatingConstMult(8, 23, 8, 23, constant, family, targetFrequency)
}

object DoublePrecisionFPConstMult {
  def apply(constant: Double, family: XilinxDeviceFamily, targetFrequency: HertzNumber) =
    FloatingConstMult(11, 52, 11, 52, constant, family, targetFrequency)
}

object FloatingConstMultSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withFstWave.compile(SinglePrecisionFPConstMult(Math.PI, UltraScale, 200 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Float = 0.0f

      // normal test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = (dut.x.toFloat * Math.PI.toFloat).abs
        if (((dut.z.toFloat - expected) / expected) >= 1e-5)
          simFailure(s"FPConstMult error, z=${dut.z.toFloat}, expected=${expected}") // precision
      }

      //special test
      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.clockDomain.waitSampling(2)
        expected = (dut.x.toFloat * Math.PI.toFloat).abs
        if (((dut.z.toFloat - expected) / expected) >= 1e-5)
          simFailure(s"FPLog error, x=${dut.x.toFloat}, z=${dut.z.toFloat}, expected=${expected}") // precision
      }

      dut.x #= 0.0f
      dut.clockDomain.waitSampling(2)
      if (!((dut.z.mantissa.toInt == 0) && (dut.z.exponent.toInt == 0))) simFailure("error")

      dut.x #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!((dut.z.mantissa.toInt == 0) && (dut.z.exponent.toInt == 0)))
        simFailure(s"FPConstMult error, z=${dut.z.toFloat}")

//      dut.x #= Float.NaN
//      dut.clockDomain.waitSampling(2)
//      simFailure("error")

      println("=========== SinglePrecision FPConstMult test success!!! ===========")
    }

    SimConfig.withFstWave.compile(DoublePrecisionFPConstMult(Math.PI, UltraScale, 200 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Double = 0.0

      // normal test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = (dut.x.toDouble * Math.PI).abs
        if (((dut.z.toDouble - expected) / expected) >= 1e-5)
          simFailure(s"FPConstMult error, z=${dut.z.toDouble}, expected=${expected}") // precision
      }

      //special test
      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.clockDomain.waitSampling(2)
        expected = (dut.x.toDouble * Math.PI).abs
        if (((dut.z.toDouble - expected) / expected) >= 1e-5)
          simFailure(s"FPLog error, x=${dut.x.toDouble}, z=${dut.z.toDouble}, expected=${expected}") // precision
      }

      dut.x #= 0.0
      dut.clockDomain.waitSampling(2)
      if (!((dut.z.mantissa.toBigInt == 0) && (dut.z.exponent.toInt == 0))) simFailure("error")

      dut.x #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!((dut.z.mantissa.toBigInt == 0) && (dut.z.exponent.toInt == 0)))
        simFailure(s"FPConstMult error, z=${dut.z.toDouble}")

      //      dut.x #= Double.NaN
      //      dut.clockDomain.waitSampling(2)
      //      simFailure("error")

      println("=========== DoublePrecision FPConstMult test success!!! ===========")
    }
  }
}
