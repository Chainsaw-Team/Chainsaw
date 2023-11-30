package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.{UltraScale, XilinxDeviceFamily}
import Chainsaw.edaFlow.XilinxDevice
import Chainsaw.{FLOPOCO, doCmd, pow2}
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class IEEEAdd (exponentSize: Int, mantissaSize: Int, family: XilinxDeviceFamily, targetFrequency: HertzNumber) extends BlackBox with floatingFlopoco{
  override val operatorName = "IEEEAdd"
  override val entityName = operatorName
  override val params = Seq(("wE", exponentSize), ("wF", mantissaSize))

  val clk = in Bool()
  mapClockDomain(clock=clk)
  val X, Y = in Bits((exponentSize+mantissaSize+1) bits)
  val R = out Bits((exponentSize+mantissaSize+1) bits)
  addRTLPath(this.verilogFile.getAbsolutePath)
  setDefinitionName(this.moduleName)
}

case class FloatingAddWithoutConvert(exponentSize: Int, mantissaSize: Int, family: XilinxDeviceFamily, targetFrequency: HertzNumber) extends Module{
  val x, y = in (Floating(exponentSize, mantissaSize))
  val z = out (Floating(exponentSize, mantissaSize))

  Seq(x,y,z).foreach(_.flattenForeach(_.simPublic()))

  val add = IEEEAdd(exponentSize, mantissaSize, family, targetFrequency)

  add.X := x.asBits
  add.Y := y.asBits
  z.assignFromBits(RegNext(add.R))
}

object SinglePrecisionFPAddWithoutConvert {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingAddWithoutConvert(8, 23, family, targetFrequency)
}

object DoublePrecisionFPAddWithoutConvert {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingAddWithoutConvert(11, 52, family, targetFrequency)
}

object FloatingAddWithoutConvertSim{
  def main(args: Array[String]): Unit = {
    SimConfig.withFstWave.compile(SinglePrecisionFPAddWithoutConvert(UltraScale, 20 MHz)).doSim{dut=>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Float = 0.0f

      // normal test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toFloat + dut.y.toFloat
        if (!(((expected - dut.z.toFloat).abs < 1e-37) || (expected.isInfinite && dut.z.isInf))) simFailure(s"FPAdd error, x=${dut.x.toFloat}, y=${dut.y.toFloat}, z=${dut.z.toFloat}, expected=${expected}")
      }

      // special test
      for (_ <- 0 until 10000) {
        dut.x.randDenormal
        dut.y.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toFloat + dut.y.toFloat
        if ((dut.z.toFloat - expected).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toFloat}, expected=${expected}") // precision
      }

      dut.x #= 0.0f
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isInf) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Float.PositiveInfinity
        dut.clockDomain.waitSampling(2)
        if (!dut.z.isInf) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
      }

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Float.NaN
        dut.clockDomain.waitSampling(2)
        if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
      }

      dut.x #= Float.PositiveInfinity
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      dut.x #= 0.0f
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      for (_ <- 0 until 100) {
        dut.x.randDenormal
        dut.y.randDenormal
        expected = dut.x.toFloat + dut.y.toFloat
        dut.clockDomain.waitSampling(2)
        if ((expected - dut.z.toFloat).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
      }

      dut.x #= Float.PositiveInfinity
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isInf) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      dut.x #= Float.NaN
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      for (_ <- 0 until 1000) {
        dut.x #= 0.0f
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!((dut.z.toFloat == dut.y.toFloat))) simFailure(s"FPAdd error, z=${dut.z.toFloat}, y=${dut.y.toFloat}")
      }

      println("========= pass test SinglePrecisionFPAddWithoutConvert =========")
    }

    SimConfig.withFstWave.compile(DoublePrecisionFPAddWithoutConvert(UltraScale, 20 MHz)).doSim{dut=>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Double = 0.0

      // normal test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        expected = dut.x.toDouble + dut.y.toDouble
        dut.clockDomain.waitSampling()
        if (!(((expected - dut.z.toDouble).abs < 1e-37) || (expected.isInfinite && dut.z.isInf))) simFailure(s"FPAdd error, z=${dut.z.toDouble}")
      }

      // special test
      for (_ <- 0 until 10000) {
        dut.x.randDenormal
        dut.y.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toDouble + dut.y.toDouble
        if ((dut.z.toDouble - expected).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toDouble}, expected=$expected") // precision
      }

      dut.x #= 0.0
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isInf) simFailure(s"FPAdd error, z=${dut.z.toDouble}")

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Double.PositiveInfinity
        dut.clockDomain.waitSampling(2)
        if (!dut.z.isInf) simFailure(s"FPAdd error, z=${dut.z.toDouble}")
      }

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Double.NaN
        dut.clockDomain.waitSampling(2)
        if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toDouble}")
      }

      dut.x #= Double.PositiveInfinity
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toDouble}")

      dut.x #= 0.0
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toDouble}")

      for (_ <- 0 until 100) {
        dut.x.randDenormal
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toDouble + dut.y.toDouble
        if ((expected - dut.z.toDouble).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toDouble}, expected=${expected}") // precision
      }

      dut.x #= Double.PositiveInfinity
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isInf) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      dut.x #= Double.NaN
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      for (_ <- 0 until 1000) {
        dut.x #= 0.0
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (dut.z.toFloat > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
      }

      println("========= pass test DoublePrecisionFPAddWithoutConvert =========")
    }
  }
}
