package Chainsaw.arithmetic.floating

import Chainsaw.edaFlow.UltraScale
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.IntToBuilder
import spinal.core.sim.{SimClockDomainHandlePimper, SimConfig, simFailure}
import spinal.lib._
import spinal.core._
import spinal.lib.fsm._
import spinal.core.sim._

import scala.math.Fractional.Implicits.infixFractionalOps

class FloatingTest extends AnyFlatSpec {
  // floating adder
  SimConfig.withFstWave.compile(SinglePrecisionFPAdd(UltraScale, 200 MHz)).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Float = 0.0f

    // normal test
    for (_ <- 0 until 10000) {
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat + dut.y.toFloat
      if (!(((expected - dut.z.toFloat).abs < 1e-37) || (expected.isInfinite && dut.z.isNaNSim)))
        simFailure(s"FPAdd error, z=${dut.z.toFloat}, expected=$expected, x=${dut.x.toFloat}, y=${dut.y.toFloat}")
    }

    // special test
    for (_ <- 0 until 10000) {
      dut.x.randDenormal
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat + dut.y.toFloat
      if ((dut.z.toFloat - expected).abs > 1e-37) simFailure(s"FPAdd error") // precision
    }

    dut.x #= 0.0f
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPAdd error")

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isInfSim) simFailure(s"FPAdd error")
    }

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isNaNSim) simFailure(s"FPAdd error")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error")

    dut.x #= 0.0f
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error")

    for (_ <- 0 until 100) {
      dut.x.randDenormal
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat + dut.y.toFloat
      if ((expected - dut.z.toFloat).abs > 1e-37) simFailure(s"FPAdd error")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPAdd error")

    dut.x #= Float.NaN
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error")

    for (_ <- 0 until 10000) {
      dut.x #= 0.0f
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!((dut.z.toFloat == dut.y.toFloat) || dut.z.isZeroSim)) simFailure(s"FPAdd error}")
    }

    println("========= pass test SinglePrecisionFPAdd =========")
  }
  SimConfig.withFstWave.compile(DoublePrecisionFPAdd(UltraScale, 200 MHz)).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Double = 0.0

    // normal test
    for (_ <- 0 until 10000) {
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble + dut.y.toDouble
      if (!(((expected - dut.z.toDouble).abs < 1e-37) || (expected.isInfinite && dut.z.isNaNSim)))
        simFailure(s"FPAdd error, z=${dut.z.toDouble}, expected=$expected, x=${dut.x.toDouble}, y=${dut.y.toDouble}")
    }

    // special test
    for (_ <- 0 until 10000) {
      dut.x.randDenormal
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble + dut.y.toDouble
      if ((dut.z.toDouble - expected).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toDouble}, expected=$expected") // precision
    }

    dut.x #= 0.0
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isInfSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")
    }

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")

    dut.x #= 0.0
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")

    for (_ <- 0 until 100) {
      dut.x.randDenormal
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble + dut.y.toDouble
      if ((expected - dut.z.toDouble).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toDouble}, expected=${expected}") // precision
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

    dut.x #= Double.NaN
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

    for (_ <- 0 until 1000) {
      dut.x #= 0.0
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!((dut.z.toDouble == dut.y.toDouble) || dut.z.isZeroSim)) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
    }

    println("========= pass test DoublePrecisionFPAdd =========")
  }

  // floating sub
  SimConfig.withFstWave.compile(SinglePrecisionFPSub(UltraScale, 200 MHz)).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Float = 0.0f

    // normal test
    for (_ <- 0 until 10000) {
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat - dut.y.toFloat
      if (!(((expected - dut.z.toFloat).abs < 1e-37) || (expected.isInfinite && dut.z.isNaNSim)))
        simFailure(s"FPSub error, x=${dut.x.toFloat}, y=${dut.y.toFloat}, z=${dut.z.toFloat}, expected=${expected}")
    }

    // special test
    for (_ <- 0 until 10000) {
      dut.x.randDenormal
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat - dut.y.toFloat
      if ((dut.z.toFloat - expected).abs > 1e-37)
        simFailure(s"FPSub error, z=${dut.z.toFloat}, expected=${expected}") // precision
    }

    dut.x #= 0.0f
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPSub error, z=${dut.z.toFloat}")

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isInfSim) simFailure(s"FPSub error, z=${dut.z.toFloat}")
    }

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toFloat}")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toFloat}")

    dut.x #= 0.0f
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toFloat}")

    for (_ <- 0 until 100) {
      dut.x.randDenormal
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat - dut.y.toFloat
      if ((expected - dut.z.toFloat).abs > 1e-37) simFailure(s"FPSub error, z=${dut.z.toFloat}")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toFloat}")

    dut.x #= Float.NaN
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toFloat}")

    for (_ <- 0 until 1000) {
      dut.x #= 0.0f
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!((dut.z.toFloat == -dut.y.toFloat) || dut.z.isZeroSim)) simFailure(s"FPSub error, z=${dut.z.toFloat}, y=${dut.y.toFloat}")
    }

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= 0.0f
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!((dut.z.toFloat == dut.x.toFloat) || dut.z.isZeroSim)) simFailure(s"FPSub error, z=${dut.z.toFloat}, y=${dut.y.toFloat}")
    }

    println("========= pass test SinglePrecisionFPSub =========")
  }
  SimConfig.withFstWave.compile(DoublePrecisionFPSub(UltraScale, 200 MHz)).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Double = 0.0

    // normal test
    for (_ <- 0 until 10000) {
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble - dut.y.toDouble
      if (!(((expected - dut.z.toDouble).abs < 1e-37) || (expected.isInfinite && dut.z.isNaNSim))) simFailure(s"FPSub error, z=${dut.z.toDouble}, expected=$expected")
    }

    // special test
    for (_ <- 0 until 10000) {
      dut.x.randDenormal
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble - dut.y.toDouble
      if ((dut.z.toDouble - expected).abs > 1e-37) simFailure(s"FPSub error, z=${dut.z.toDouble}, expected=$expected") // precision
    }

    dut.x #= 0.0
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPSub error, z=${dut.z.toDouble}")

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isInfSim) simFailure(s"FPSub error, z=${dut.z.toDouble}")
    }

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toDouble}")
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toDouble}")

    dut.x #= 0.0
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toDouble}")

    for (_ <- 0 until 100) {
      dut.x.randDenormal
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble - dut.y.toDouble
      if ((expected - dut.z.toDouble).abs > 1e-37) simFailure(s"FPSub error, z=${dut.z.toDouble}, expected=${expected}") // precision
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toFloat}")

    dut.x #= Double.NaN
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPSub error, z=${dut.z.toFloat}")

    for (_ <- 0 until 1000) {
      dut.x #= 0.0
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!((dut.z.toDouble == -dut.y.toDouble) || dut.z.isZeroSim)) simFailure(s"FPSub error, z=${dut.z.toFloat}")
    }

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= 0.0
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!((dut.z.toDouble == dut.x.toDouble) || dut.z.isZeroSim)) simFailure(s"FPSub error, z=${dut.z.toFloat}")
    }

    println("========= pass test DoublePrecisionFPSub =========")
  }

  // floating mult
  SimConfig.withFstWave.compile(SinglePrecisionFPMult(UltraScale, 200 MHz)).doSim{dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Float = 0.0f

    // normal test
    for(_ <- 0 until 10000){
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat * dut.y.toFloat
      if((expected-dut.z.toFloat).abs>1e-37) simFailure(s"FPMult error, z=${dut.z.toFloat}, expected=${expected}") // precision
    }

    // special test
    //      for(_ <- 0 until 10000){
    //        dut.x.randDenormal
    //        dut.y.randNormal
    //        dut.clockDomain.waitSampling(dut.latency+2)
    //        expected = dut.x.toFloat * dut.y.toFloat
    //        simFailure("error!")
    //      }

    dut.x #= 0.0f
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toFloat}")

    for(_ <- 0 until 10000){
      dut.y.randDenormal
      dut.x #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!(dut.z.isNaNSim || dut.z.isInfSim)) simFailure(s"FPMult error, z=${dut.z.toFloat}")
    }

    for(_ <- 0 until 1000){
      dut.x.randDenormal
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toFloat}")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toFloat}")

    dut.x #= 0.0f
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toFloat}")

    for(_ <- 0 until 10000){
      dut.x.randDenormal
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat * dut.y.toFloat
      if(expected!=dut.z.toFloat) simFailure(s"FPMult error, z=${dut.z.toFloat}, expected=${expected}")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isInfSim) simFailure(s"FPMult error, z=${dut.z.toFloat}")

    dut.x #= Float.NaN
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toFloat}")

    for(_ <- 0 until 1000){
      dut.x #= 0.0f
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isZeroSim) simFailure(s"FPMult error, z=${dut.z.toFloat}")
    }

    println("========= pass test SinglePrecisionFPMult =========")
  }
  SimConfig.withFstWave.compile(DoublePrecisionFPMult(UltraScale, 200 MHz)).doSim{dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Double = 0.0

    // random test
    for(_ <- 0 until 10000){
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble * dut.y.toDouble
      if((expected-dut.z.toDouble).abs>1e-37) simFailure(s"FPMult error, z=${dut.z.toDouble}, expected=${expected}") // precision
    }

    // special test
    //      for(_ <- 0 until 10000){
    //        dut.x.randDenormal
    //        dut.y.randNormal
    //        dut.clockDomain.waitSampling(dut.latency+2)
    //        expected = dut.x.toDouble * dut.y.toDouble
    //        simFailure("error") // precision
    //      }

    dut.x #= 0.0
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toDouble}")

    for(_ <- 0 until 1000){
      dut.x.randDenormal
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toDouble}")
    }

    for(_ <- 0 until 100){
      dut.x.randDenormal
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toDouble}")
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toDouble}")

    dut.x #= 0.0
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toDouble}")

    for(_ <- 0 until 10000){
      dut.x.randDenormal
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble * dut.y.toDouble
      if(expected!=dut.z.toDouble) simFailure(s"FPMult error, z=${dut.z.toDouble}, expected=${expected}")
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isInfSim) simFailure(s"FPMult error, z=${dut.z.toDouble}")

    dut.x #= Double.NaN
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPMult error, z=${dut.z.toDouble}")

    for(_ <- 0 until 1000){
      dut.x #= 0.0
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isZeroSim) simFailure(s"FPMult error, z=${dut.z.toDouble}")
    }

    println("========= pass test DoublePrecisionFPMult =========")
  }

  // floating div
  SimConfig.withFstWave.compile(SinglePrecisionFPDiv(UltraScale, 200 MHz)).doSim{dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Float = 0.0f

    // random test
    for(_ <- 0 until 10000){
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat / dut.y.toFloat
      if((expected-dut.z.toFloat).abs > 1e-37) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
    }

    // special test
    //      for(_ <- 0 until 10000) {
    //        dut.x.randDenormal
    //        dut.y.randNormal
    //        dut.clockDomain.waitSampling(dut.latency+2)
    //        expected = dut.x.toFloat / dut.y.toFloat
    //        simFailure("error")
    //      }

    //      for(_ <- 0 until 10000) {
    //        dut.x.randNormal
    //        dut.y.randDenormal
    //        dut.clockDomain.waitSampling(dut.latency+2)
    //        expected = dut.x.toFloat / dut.y.toFloat
    //        simFailure("error")
    //      }

    dut.x #= 0
    dut.y #= 0
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

    dut.x #= 0
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isZeroSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

    dut.x #= Float.PositiveInfinity
    dut.y #= 0
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isInfSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

    for(_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isZeroSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
    }

    for(_ <- 0 until 1000) {
      dut.x #= Float.PositiveInfinity
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isInfSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
    }

    for(_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
    }

    for(_ <- 0 until 1000) {
      dut.x #= Float.NaN
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

    dut.x #= Float.NaN
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

    dut.x #= 0.0f
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

    dut.x #= Float.NaN
    dut.y #= 0.0f
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

    //      for(_ <- 0 until 1000){
    //        dut.x.randDenormal
    //        dut.y.randDenormal
    //        dut.clockDomain.waitSampling(dut.latency+2)
    //        simFailure("error")
    //      }

    for(_ <- 0 until 1000){
      dut.x #= 0.0
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!(dut.z.isNaNSim || dut.z.isZeroSim)) simFailure("FPDiv error")
    }

    for(_ <- 0 until 1000){
      dut.x.randDenormal
      dut.y #= 0.0
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!(dut.z.isNaNSim || dut.z.isInfSim)) simFailure("FPDiv error")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

    dut.x #= Float.NaN
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toFloat}")

    println("pass test SinglePrecisionFPDiv")
  }
  SimConfig.withFstWave.compile(DoublePrecisionFPDiv(UltraScale, 200 MHz)).doSim{dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Double = 0.0

    // random test
    for(_ <- 0 until 10000){
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble / dut.y.toDouble
      if((expected-dut.z.toDouble).abs > 1e-37) simFailure("FPDiv error")
    }

    // special test
    //      for(_ <- 0 until 10000) {
    //        dut.x.randDenormal
    //        dut.y.randNormal
    //        dut.clockDomain.waitSampling(dut.latency+2)
    //        expected = dut.x.toDouble / dut.y.toDouble
    //        simFailure("error")
    //      }

    //      for(_ <- 0 until 10000) {
    //        dut.x.randNormal(true)
    //        dut.y.randDenormal
    //        dut.clockDomain.waitSampling(dut.latency+2)
    //        expected = dut.x.toDouble / dut.y.toDouble
    //        simFailure("error")
    //      }

    dut.x #= 0
    dut.y #= 0
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

    dut.x #= 0
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isZeroSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

    dut.x #= Double.PositiveInfinity
    dut.y #= 0
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isInfSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

    for(_ <- 0 until 1000){
      dut.x.randDenormal
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isZeroSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")
    }

    for(_ <- 0 until 1000){
      dut.x #= Double.PositiveInfinity
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isInfSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")
    }

    for(_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")
    }

    for(_ <- 0 until 1000) {
      dut.x #= Double.NaN
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

    dut.x #= Double.NaN
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

    dut.x #= 0.0
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

    dut.x #= Double.NaN
    dut.y #= 0.0
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

    for(_ <- 0 until 10000){
      dut.x.randDenormal
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble / dut.y.toDouble
      if(!dut.z.isNaNSim) simFailure("FPDiv error")
    }

    for(_ <- 0 until 1000){
      dut.x #= 0.0
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isNaNSim) simFailure("FPDiv error")
    }

    for(_ <- 0 until 1000){
      dut.x.randDenormal
      dut.y #= 0.0
      dut.clockDomain.waitSampling(dut.latency+2)
      if(!dut.z.isNaNSim) simFailure("FPDiv error")
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

    dut.x #= Double.NaN
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isNaNSim) simFailure(s"FPDiv error, z=${dut.z.toDouble}")

    println("pass test DoublePrecisionFPDiv")
  }

  // floating adder without convert
  SimConfig.withFstWave.compile(SinglePrecisionFPAddWithoutConvert(UltraScale, 200 MHz)).doSim{dut=>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Float = 0.0f

    // normal test
    for (_ <- 0 until 10000) {
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat + dut.y.toFloat
      if (!(((expected - dut.z.toFloat).abs < 1e-37) || (expected.isInfinite && dut.z.isInfSim))) simFailure(s"FPAdd error, x=${dut.x.toFloat}, y=${dut.y.toFloat}, z=${dut.z.toFloat}, expected=${expected}")
    }

    // special test
    for (_ <- 0 until 10000) {
      dut.x.randDenormal
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat + dut.y.toFloat
      if ((dut.z.toFloat - expected).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toFloat}, expected=${expected}") // precision
    }

    dut.x #= 0.0f
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Float.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isInfSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
    }

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Float.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

    dut.x #= 0.0f
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

    for (_ <- 0 until 100) {
      dut.x.randDenormal
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toFloat + dut.y.toFloat
      if ((expected - dut.z.toFloat).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
    }

    dut.x #= Float.PositiveInfinity
    dut.y #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

    dut.x #= Float.NaN
    dut.y #= Float.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

    for (_ <- 0 until 1000) {
      dut.x #= 0.0f
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!((dut.z.toFloat == dut.y.toFloat))) simFailure(s"FPAdd error, z=${dut.z.toFloat}, y=${dut.y.toFloat}")
    }

    println("========= pass test SinglePrecisionFPAddWithoutConvert =========")
  }
  SimConfig.withFstWave.compile(DoublePrecisionFPAddWithoutConvert(UltraScale, 200 MHz)).doSim{dut=>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Double = 0.0

    // normal test
    for (_ <- 0 until 10000) {
      dut.x.randNormal(true)
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble + dut.y.toDouble
      if (!(((expected - dut.z.toDouble).abs < 1e-37) || (expected.isInfinite && dut.z.isInfSim))) simFailure(s"FPAdd error, z=${dut.z.toDouble}")
    }

    // special test
    for (_ <- 0 until 10000) {
      dut.x.randDenormal
      dut.y.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble + dut.y.toDouble
      if ((dut.z.toDouble - expected).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toDouble}, expected=$expected") // precision
    }

    dut.x #= 0.0
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Double.PositiveInfinity
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isInfSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")
    }

    for (_ <- 0 until 1000) {
      dut.x.randDenormal
      dut.y #= Double.NaN
      dut.clockDomain.waitSampling(dut.latency+2)
      if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")

    dut.x #= 0.0
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toDouble}")

    for (_ <- 0 until 100) {
      dut.x.randDenormal
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = dut.x.toDouble + dut.y.toDouble
      if ((expected - dut.z.toDouble).abs > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toDouble}, expected=${expected}") // precision
    }

    dut.x #= Double.PositiveInfinity
    dut.y #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isInfSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

    dut.x #= Double.NaN
    dut.y #= Double.NaN
    dut.clockDomain.waitSampling(dut.latency+2)
    if (!dut.z.isNaNSim) simFailure(s"FPAdd error, z=${dut.z.toFloat}")

    for (_ <- 0 until 1000) {
      dut.x #= 0.0
      dut.y.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      if (dut.z.toFloat > 1e-37) simFailure(s"FPAdd error, z=${dut.z.toFloat}")
    }

    println("========= pass test DoublePrecisionFPAddWithoutConvert =========")
  }

  // floating ConstMult
  SimConfig.withFstWave.compile(SinglePrecisionFPConstMult(Math.PI, UltraScale, 200 MHz)).doSim{dut=>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Float = 0.0f

    // normal test
    for(_ <- 0 until 10000){
      dut.x.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = (dut.x.toFloat * Math.PI.toFloat).abs
      if(((dut.z.toFloat-expected)/expected)>=1e-5) simFailure(s"FPConstMult error, z=${dut.z.toFloat}, expected=${expected}") // precision
    }

    //special test
    for(_ <- 0 until 1000){
      dut.x.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = (dut.x.toFloat * Math.PI.toFloat).abs
      if(((dut.z.toFloat-expected)/expected)>=1e-5) simFailure(s"FPLog error, x=${dut.x.toFloat}, z=${dut.z.toFloat}, expected=${expected}") // precision
    }

    dut.x #= 0.0f
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isZeroSim) simFailure("error")

    dut.x #= Float.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isInfSim) simFailure(s"FPConstMult error, z=${dut.z.toFloat}")

    //      dut.x #= Float.NaN
    //      dut.clockDomain.waitSampling(dut.latency+2)
    //      simFailure("error")

    println("=========== SinglePrecision FPConstMult test success!!! ===========")
  }
  SimConfig.withFstWave.compile(DoublePrecisionFPConstMult(Math.PI, UltraScale, 200 MHz)).doSim{dut=>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()
    var expected: Double = 0.0

    // normal test
    for(_ <- 0 until 10000){
      dut.x.randNormal(true)
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = (dut.x.toDouble * Math.PI).abs
      if(((dut.z.toDouble-expected)/expected)>=1e-5) simFailure(s"FPConstMult error, z=${dut.z.toDouble}, expected=${expected}") // precision
    }

    //special test
    for(_ <- 0 until 1000){
      dut.x.randDenormal
      dut.clockDomain.waitSampling(dut.latency+2)
      expected = (dut.x.toDouble * Math.PI).abs
      if(((dut.z.toDouble-expected)/expected)>=1e-5) simFailure(s"FPLog error, x=${dut.x.toDouble}, z=${dut.z.toDouble}, expected=${expected}") // precision
    }

    dut.x #= 0.0
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isZeroSim) simFailure("error")

    dut.x #= Double.PositiveInfinity
    dut.clockDomain.waitSampling(dut.latency+2)
    if(!dut.z.isInfSim) simFailure(s"FPConstMult error, z=${dut.z.toDouble}")

    //      dut.x #= Double.NaN
    //      dut.clockDomain.waitSampling(dut.latency+2)
    //      simFailure("error")

    println("=========== DoublePrecision FPConstMult test success!!! ===========")
  }
}
