package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.{UltraScale, XilinxDeviceFamily}
import Chainsaw.xilinx.XilinxDevice
import Chainsaw.{FLOPOCO, doCmd, pow2}
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class FlopocoAdd(exponentSize: Int, mantissaSize: Int, sub: Boolean, override val family: XilinxDeviceFamily, override val targetFrequency: HertzNumber) extends BlackBox with floatingFlopoco {
  override val operatorName = "FPAdd"
  override val entityName = if(sub) "FPSub" else "FPAdd"
  override val params = Seq(("wE", exponentSize), ("wF", mantissaSize), ("sub", sub))

  val clk = in Bool()
  mapClockDomain(clock = clk)
  val X, Y = in Bits(exponentSize+mantissaSize+1+2 bits)
  val R = out Bits(exponentSize+mantissaSize+1+2 bits)
  addRTLPath(this.verilogFile.getAbsolutePath)
  setDefinitionName(this.moduleName)
}

case class FloatingAdd(exponentSize: Int, mantissaSize: Int, family: XilinxDeviceFamily, targetFrequency: HertzNumber) extends Module{
  val x, y = in (Floating(exponentSize, mantissaSize))
  val z = out (Floating(exponentSize, mantissaSize))

  Seq(x,y,z).foreach(_.flattenForeach(_.simPublic()))

  val i2f0, i2f1 = Ieee2Flopoco(exponentSize, mantissaSize, family, targetFrequency)
  val f2i = Flopoco2Ieee(exponentSize, mantissaSize, family, targetFrequency)
  val add = FlopocoAdd(exponentSize, mantissaSize, false, family, targetFrequency)

  i2f0.X := x.asBits
  i2f1.X := y.asBits
  add.X := i2f0.R
  add.Y := i2f1.R
  f2i.X := add.R
  z.assignFromBits(RegNext(f2i.R)) // latency = 1
}

case class FloatingSub(exponentSize: Int, mantissaSize: Int, family: XilinxDeviceFamily, targetFrequency: HertzNumber) extends Module{
  val x, y = in (Floating(exponentSize, mantissaSize))
  val z = out (Floating(exponentSize, mantissaSize))

  Seq(x,y,z).foreach(_.flattenForeach(_.simPublic()))

  val i2f0, i2f1 = Ieee2Flopoco(exponentSize, mantissaSize, family, targetFrequency)
  val f2i = Flopoco2Ieee(exponentSize, mantissaSize, family, targetFrequency)
  val add = FlopocoAdd(exponentSize, mantissaSize, true, family, targetFrequency)

  i2f0.X := x.asBits
  i2f1.X := y.asBits
  add.X := i2f0.R
  add.Y := i2f1.R
  f2i.X := add.R
  z.assignFromBits(RegNext(f2i.R)) // latency = 1
}

object SinglePrecisionFPAdd {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingAdd(8, 23, family, targetFrequency)
}

object DoublePrecisionFPAdd {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingAdd(11, 52, family, targetFrequency)
}

object SinglePrecisionFPSub {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingSub(8, 23, family, targetFrequency)
}

object DoublePrecisionFPSub {
  def apply(family:XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingSub(11, 52, family, targetFrequency)
}

object FloatingAddSim {
  def main(args: Array[String]): Unit = {
    // floating adder
    SimConfig.withFstWave.compile(SinglePrecisionFPAdd(UltraScale, 20 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Float = 0.0f

      // normal test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toFloat + dut.y.toFloat
        if (!(((expected - dut.z.toFloat).abs < 1e-37) || (expected.isInfinite && dut.z.isNaN))) simFailure(s"FPAdd error, x=${dut.x.toFloat}, y=${dut.y.toFloat}, z=${dut.z.toFloat}, expected=${expected}")
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

      for (_ <- 0 until 10000) {
        dut.x #= 0.0f
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!((dut.z.toFloat == dut.y.toFloat) || (dut.z.toFloat == 0))) simFailure(s"FPAdd error, z=${dut.z.toFloat}, y=${dut.y.toFloat}")
      }

      println("========= pass test SinglePrecisionFPAdd =========")
    }
    SimConfig.withFstWave.compile(DoublePrecisionFPAdd(UltraScale, 20 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Double = 0.0

      // normal test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        expected = dut.x.toDouble + dut.y.toDouble
        dut.clockDomain.waitSampling()
        if (!(((expected - dut.z.toDouble).abs < 1e-37) || (expected.isInfinite && dut.z.isNaN))) simFailure(s"FPAdd error, z=${dut.z.toDouble}")
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
        if (!((dut.z.toDouble == dut.y.toDouble) || (dut.z.toDouble==0))) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
      }

      println("========= pass test DoublePrecisionFPAdd =========")
    }

    // floating sub
    SimConfig.withFstWave.compile(SinglePrecisionFPSub(UltraScale, 20 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Float = 0.0f

      // normal test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toFloat - dut.y.toFloat
        if (!(((expected - dut.z.toFloat).abs < 1e-37) || (expected.isInfinite && dut.z.isNaN))) simFailure(s"FPAdd error, x=${dut.x.toFloat}, y=${dut.y.toFloat}, z=${dut.z.toFloat}, expected=${expected}")
      }

      // special test
      for (_ <- 0 until 10000) {
        dut.x.randDenormal
        dut.y.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toFloat - dut.y.toFloat
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
        expected = dut.x.toFloat - dut.y.toFloat
        dut.clockDomain.waitSampling(2)
        if ((expected - dut.z.toFloat).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
      }

      dut.x #= Float.PositiveInfinity
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      dut.x #= Float.NaN
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      for (_ <- 0 until 1000) {
        dut.x #= 0.0f
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!((dut.z.toFloat == -dut.y.toFloat) || (dut.z.toFloat == 0))) simFailure(s"FPAdd error, z=${dut.z.toFloat}, y=${dut.y.toFloat}")
      }

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= 0.0f
        dut.clockDomain.waitSampling(2)
        if (!((dut.z.toFloat == dut.x.toFloat) || (dut.z.toFloat == 0))) simFailure(s"FPAdd error, z=${dut.z.toFloat}, y=${dut.y.toFloat}")
      }

      println("========= pass test SinglePrecisionFPSub =========")
    }
    SimConfig.withFstWave.compile(DoublePrecisionFPSub(UltraScale, 20 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Double = 0.0

      // normal test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        expected = dut.x.toDouble - dut.y.toDouble
        dut.clockDomain.waitSampling()
        if (!(((expected - dut.z.toDouble).abs < 1e-37) || (expected.isInfinite && dut.z.isNaN))) simFailure(s"FPAdd error, z=${dut.z.toDouble}, expected=$expected")
      }

      // special test
      for (_ <- 0 until 10000) {
        dut.x.randDenormal
        dut.y.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toDouble - dut.y.toDouble
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
        expected = dut.x.toDouble - dut.y.toDouble
        if ((expected - dut.z.toDouble).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toDouble}, expected=${expected}") // precision
      }

      dut.x #= Double.PositiveInfinity
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      dut.x #= Double.NaN
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

      for (_ <- 0 until 1000) {
        dut.x #= 0.0
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!((dut.z.toDouble == -dut.y.toDouble) || (dut.z.toDouble==0))) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
      }

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= 0.0
        dut.clockDomain.waitSampling(2)
        if (!((dut.z.toDouble == dut.x.toDouble) || (dut.z.toDouble==0))) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
      }

      println("========= pass test DoublePrecisionFPSub =========")
    }
  }
}
