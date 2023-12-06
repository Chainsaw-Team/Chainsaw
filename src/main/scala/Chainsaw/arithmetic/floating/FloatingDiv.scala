package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.Device._
import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class FlopocoDiv(
    exponentSize: Int,
    mantissaSize: Int,
    override val family: XilinxDeviceFamily,
    override val targetFrequency: HertzNumber
) extends BlackBox
    with floatingFlopoco {
  override val operatorName = "FPDiv"
  override val entityName   = "FPDiv"
  override val params       = Seq(("wE", exponentSize), ("wF", mantissaSize))

  val clk = in Bool ()
  mapClockDomain(clock = clk)
  val X, Y = in Bits (exponentSize + mantissaSize + 1 + 2 bits)
  val R    = out Bits (exponentSize + mantissaSize + 1 + 2 bits)

  addRTLPath(this.verilogFile.getAbsolutePath)
  setDefinitionName(this.moduleName)
}

//abstract class FloatingBinaryOp(
//    exponentSize: Int,
//    mantissaSize: Int,
//    family: XilinxDeviceFamily,
//    targetFrequency: HertzNumber
//) extends Module {
//  val x, y = in(Floating(exponentSize, mantissaSize))
//  val z    = out(Floating(exponentSize, mantissaSize))
//  val core: BlackBox with floatingFlopoco
//  Seq(x, y, z).foreach(_.flattenForeach(_.simPublic()))
//
//  val i2f0, i2f1 = Ieee2Flopoco(exponentSize, mantissaSize, family, targetFrequency)
//  val f2i        = Flopoco2Ieee(exponentSize, mantissaSize, family, targetFrequency)
//  i2f0.X := x.asBits
//  i2f1.X := y.asBits
//  core.X := i2f0.R
//  core.Y := i2f1.R
//  f2i.X  := core.R
//  z.assignFromBits(RegNext(f2i.R)) // latency = 1
//
//  def impl(inputs: Seq[Floating]): Seq[Floating] = {
//    val op = this
//    op.x := inputs(0)
//    op.y := inputs(1)
//    Seq(op.z)
//  }
//}
//
//case class FloatingDivAnother(
//    exponentSize: Int,
//    mantissaSize: Int,
//    family: XilinxDeviceFamily,
//    targetFrequency: HertzNumber
//) extends FloatingBinaryOp(exponentSize, mantissaSize, family, targetFrequency) {
//  override val core = FlopocoDiv(exponentSize, mantissaSize, family, targetFrequency)
//}

case class FloatingDiv(exponentSize: Int, mantissaSize: Int, family: XilinxDeviceFamily, targetFrequency: HertzNumber)
    extends Module {
  val x, y = in(Floating(exponentSize, mantissaSize))
  val z    = out(Floating(exponentSize, mantissaSize))

  Seq(x, y, z).foreach(_.flattenForeach(_.simPublic()))

  val i2f0, i2f1 = Ieee2Flopoco(exponentSize, mantissaSize, family, targetFrequency)
  val f2i        = Flopoco2Ieee(exponentSize, mantissaSize, family, targetFrequency)
  val add        = FlopocoDiv(exponentSize, mantissaSize, family, targetFrequency)

  i2f0.X := x.asBits
  i2f1.X := y.asBits
  add.X  := i2f0.R
  add.Y  := i2f1.R
  f2i.X  := add.R
  z.assignFromBits(RegNext(f2i.R)) // latency = 1
}

object SinglePrecisonFPDiv {
  def apply(family: XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingDiv(8, 23, family, targetFrequency)
}

object DoublePrecisionFPDiv {
  def apply(family: XilinxDeviceFamily, targetFrequency: HertzNumber) = FloatingDiv(11, 52, family, targetFrequency)
}

object FloatingDivSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withFstWave.compile(SinglePrecisonFPDiv(UltraScale, 10 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Float = 0.0f

      // random test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        expected = dut.x.toFloat / dut.y.toFloat
        dut.clockDomain.waitSampling()
        if ((expected - dut.z.toFloat).abs > 1e-37) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
      }

      // special test
      //      for(_ <- 0 until 10000) {
      //        dut.x.randDenormal
      //        dut.y.randNormal
      //        dut.clockDomain.waitSampling(2)
      //        expected = dut.x.toFloat / dut.y.toFloat
      //        simFailure("error")
      //      }

      //      for(_ <- 0 until 10000) {
      //        dut.x.randNormal
      //        dut.y.randDenormal
      //        dut.clockDomain.waitSampling(2)
      //        expected = dut.x.toFloat / dut.y.toFloat
      //        simFailure("error")
      //      }

      dut.x #= 0
      dut.y #= 0
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toFloat.isNaN) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

      dut.x #= 0
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (dut.z.toFloat != 0.0f) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

      dut.x #= Float.PositiveInfinity
      dut.y #= 0
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toFloat.isInfinite) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Float.PositiveInfinity
        dut.clockDomain.waitSampling(2)
        if (dut.z.toFloat != 0) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
      }

      for (_ <- 0 until 1000) {
        dut.x #= Float.PositiveInfinity
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!dut.z.toFloat.isInfinite) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
      }

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Float.NaN
        dut.clockDomain.waitSampling(2)
        if (!dut.z.toFloat.isNaN) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
      }

      for (_ <- 0 until 1000) {
        dut.x #= Float.NaN
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!dut.z.toFloat.isNaN) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
      }

      dut.x #= Float.PositiveInfinity
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toFloat.isNaN) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

      dut.x #= Float.NaN
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toFloat.isNaN) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

      dut.x #= 0.0f
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toFloat.isNaN) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

      dut.x #= Float.NaN
      dut.y #= 0.0f
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toFloat.isNaN) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

      //      for(_ <- 0 until 1000){
      //        dut.x.randDenormal
      //        dut.y.randDenormal
      //        dut.clockDomain.waitSampling(2)
      //        simFailure("error")
      //      }

      for (_ <- 0 until 1000) {
        dut.x #= 0.0
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!(dut.z.toFloat.isNaN || dut.z.toFloat == 0.0)) simFailure("FPDiv error")
      }

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= 0.0
        dut.clockDomain.waitSampling(2)
        if (!(dut.z.toFloat.isNaN || dut.z.toFloat.isInfinite)) simFailure("FPDiv error")
      }

      dut.x #= Float.PositiveInfinity
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toFloat.isNaN) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

      dut.x #= Float.NaN
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toFloat.isNaN) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

      println("pass test SinglePrecisionFPDiv")
    }
    SimConfig.withFstWave.compile(DoublePrecisionFPDiv(UltraScale, 10 MHz)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      var expected: Double = 0.0

      // random test
      for (_ <- 0 until 10000) {
        dut.x.randNormal(true)
        dut.y.randNormal(true)
        expected = dut.x.toDouble / dut.y.toDouble
        dut.clockDomain.waitSampling()
        if ((expected - dut.z.toDouble).abs > 1e-37) simFailure("FPDiv error")
      }

      // special test
      //      for(_ <- 0 until 10000) {
      //        dut.x.randDenormal
      //        dut.y.randNormal
      //        dut.clockDomain.waitSampling(2)
      //        expected = dut.x.toDouble / dut.y.toDouble
      //        simFailure("error")
      //      }

//      for(_ <- 0 until 10000) {
//        dut.x.randNormal(true)
//        dut.y.randDenormal
//        dut.clockDomain.waitSampling(2)
//        expected = dut.x.toDouble / dut.y.toDouble
//        simFailure("error")
//      }

      dut.x #= 0
      dut.y #= 0
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toDouble.isNaN) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

      dut.x #= 0
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (dut.z.toDouble != 0.0) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

      dut.x #= Double.PositiveInfinity
      dut.y #= 0
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toDouble.isInfinite) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Double.PositiveInfinity
        dut.clockDomain.waitSampling(2)
        if (dut.z.toDouble != 0) simFailure(s"FPDiv error, z=${dut.z.toDouble}")
      }

      for (_ <- 0 until 1000) {
        dut.x #= Double.PositiveInfinity
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!dut.z.toDouble.isInfinite) simFailure(s"FPDiv error, z=${dut.z.toDouble}")
      }

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= Double.NaN
        dut.clockDomain.waitSampling(2)
        if (!dut.z.toDouble.isNaN) simFailure(s"FPDiv error, z=${dut.z.toDouble}")
      }

      for (_ <- 0 until 1000) {
        dut.x #= Double.NaN
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!dut.z.toDouble.isNaN) simFailure(s"FPDiv error, z=${dut.z.toDouble}")
      }

      dut.x #= Double.PositiveInfinity
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toDouble.isNaN) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

      dut.x #= Double.NaN
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toDouble.isNaN) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

      dut.x #= 0.0
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toDouble.isNaN) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

      dut.x #= Double.NaN
      dut.y #= 0.0
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toDouble.isNaN) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

      for (_ <- 0 until 10000) {
        dut.x.randDenormal
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        expected = dut.x.toDouble / dut.y.toDouble
        if (!dut.z.isNaN) simFailure("FPDiv error")
      }

      for (_ <- 0 until 1000) {
        dut.x #= 0.0
        dut.y.randDenormal
        dut.clockDomain.waitSampling(2)
        if (!dut.z.toDouble.isNaN) simFailure("FPDiv error")
      }

      for (_ <- 0 until 1000) {
        dut.x.randDenormal
        dut.y #= 0.0
        dut.clockDomain.waitSampling(2)
        if (!dut.z.toDouble.isNaN) simFailure("FPDiv error")
      }

      dut.x #= Double.PositiveInfinity
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toDouble.isNaN) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

      dut.x #= Double.NaN
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(2)
      if (!dut.z.toDouble.isNaN) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

      println("pass test DoublePrecisionFPDiv")
    }
  }
}
