package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.{UltraScale, XilinxDeviceFamily}
import Chainsaw.edaFlow.XilinxDevice
import Chainsaw.{FLOPOCO, doCmd, pow2}
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class FlopocoMult(
    exponentSize: Int,
    mantissaSize: Int,
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber
) extends BlackBox
    with floatingFlopoco {
  override val operatorName = "FPMult"
  override val entityName   = "FPMult"
  override val params       = Seq(("wE", exponentSize), ("wF", mantissaSize))

  val clk = in Bool ()
  mapClockDomain(clock = clk)
  val X, Y = in Bits (exponentSize + mantissaSize + 1 + 2 bits)
  val R    = out Bits (exponentSize + mantissaSize + 1 + 2 bits)

  addRTLPath(this.verilogFile.getAbsolutePath)
  setDefinitionName(this.moduleName)
}

case class FloatingMult(exponentSize: Int, mantissaSize: Int, family: XilinxDeviceFamily, targetFrequency: HertzNumber)
    extends Module {
  val x, y = in(Floating(exponentSize, mantissaSize))
  val z    = out(Floating(exponentSize, mantissaSize))

  Seq(x, y, z).foreach(_.flattenForeach(_.simPublic()))

  val i2f0, i2f1 = Ieee2Flopoco(exponentSize, mantissaSize, family, targetFrequency)
  val f2i        = Flopoco2Ieee(exponentSize, mantissaSize, family, targetFrequency)
  val add        = FlopocoMult(exponentSize, mantissaSize, family, targetFrequency)

  i2f0.X := x.asBits
  i2f1.X := y.asBits
  add.X  := i2f0.R
  add.Y  := i2f1.R
  f2i.X  := add.R
  z.assignFromBits(RegNext(f2i.R)) // latency = 1
}

object SinglePrecisionFPMult {
  def apply(family: XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingMult(8, 23, family, targetFrequency)
}

object DoublePrecisionFPMult {
  def apply(family: XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingMult(11, 52, family, targetFrequency)
}

object FloatingMultSim {
  def main(args: Array[String]): Unit = {
    // floating multiplier
    SimConfig.withFstWave.compile(SinglePrecisionFPMult(UltraScale, 10 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Float = 0.0f

      // normal test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toFloat * dut.y.toFloat
        if ((expected - dut.z.toFloat).abs > 1e-37)
          simFailure(s"FPMult error, z=${dut.z.toFloat}, expected=${expected}") // precision
      }

      // special test
      //      for(_ <- 0 until 10000){
      //        dut.x.randDenormal
      //        dut.y.randNormal
      //        dut.clockDomain.waitSampling(2)
      //        expected = dut.x.toFloat * dut.y.toFloat
      //        simFailure("error!")
      //      }

      dut.x #= 0.0f
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPMult error, z=${dut.z.toFloat}")

      for (_ <- 0 until 10000) {
        dut.y.randDenormal
        dut.x #= Float.PositiveInfinity
        dut.clockDomain.waitSampling(2)
        if (!(dut.z.isNaN || dut.z.isInf)) simFailure(s"FPMult error, z=${dut.z.toFloat}")
      }

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Float.NaN
        dut.clockDomain.waitSampling(2)
        if (!dut.z.isNaN) simFailure(s"FPMult error, z=${dut.z.toFloat}")
      }

      dut.x #= Float.PositiveInfinity
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPMult error, z=${dut.z.toFloat}")

      dut.x #= 0.0f
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toFloat.isNaN) simFailure(s"FPMult error, z=${dut.z.toFloat}")

      for (_ <- 0 until 10000) {
        dut.x.randDenormal
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toFloat * dut.y.toFloat
        if (expected != dut.z.toFloat) simFailure(s"FPMult error, z=${dut.z.toFloat}, expected=${expected}")
      }

      dut.x #= Float.PositiveInfinity
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isInf) simFailure(s"FPMult error, z=${dut.z.toFloat}")

      dut.x #= Float.NaN
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPMult error, z=${dut.z.toFloat}")

      for (_ <- 0 until 1000) {
        dut.x #= 0.0f
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (dut.z.toFloat != 0) simFailure(s"FPMult error, z=${dut.z.toFloat}")
      }

      println("========= pass test SinglePrecisionFPMult =========")
    }
    SimConfig.withFstWave.compile(DoublePrecisionFPMult(UltraScale, 10 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Double = 0.0

      // random test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toDouble * dut.y.toDouble
        if ((expected - dut.z.toDouble).abs > 1e-37)
          simFailure(s"FPMult error, z=${dut.z.toDouble}, expected=${expected}") // precision
      }

      // special test
      //      for(_ <- 0 until 10000){
      //        dut.x.randDenormal
      //        dut.y.randNormal
      //        dut.clockDomain.waitSampling(2)
      //        expected = dut.x.toDouble * dut.y.toDouble
      //        simFailure("error") // precision
      //      }

      dut.x #= 0.0
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toDouble.isNaN) simFailure(s"FPMult error, z=${dut.z.toDouble}")

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Double.PositiveInfinity
        dut.clockDomain.waitSampling(2)
        if (!dut.z.isNaN) simFailure(s"FPMult error, z=${dut.z.toDouble}")
      }

      for (_ <- 0 until 100) {
        dut.x.randDenormal
        dut.y #= Double.NaN
        dut.clockDomain.waitSampling(2)
        if (!dut.z.isNaN) simFailure(s"FPMult error, z=${dut.z.toDouble}")
      }

      dut.x #= Double.PositiveInfinity
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPMult error, z=${dut.z.toDouble}")

      dut.x #= 0.0
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPMult error, z=${dut.z.toDouble}")

      for (_ <- 0 until 10000) {
        dut.x.randDenormal
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toDouble * dut.y.toDouble
        if (expected != dut.z.toDouble) simFailure(s"FPMult error, z=${dut.z.toDouble}, expected=${expected}")
      }

      dut.x #= Double.PositiveInfinity
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isInf) simFailure(s"FPMult error, z=${dut.z.toDouble}")

      dut.x #= Double.NaN
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.isNaN) simFailure(s"FPMult error, z=${dut.z.toDouble}")

      for (_ <- 0 until 1000) {
        dut.x #= 0.0
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (dut.x.toDouble != 0) simFailure(s"FPMult error, z=${dut.z.toDouble}")
      }

      println("========= pass test DoublePrecisionFPMult =========")
    }
  }
}
