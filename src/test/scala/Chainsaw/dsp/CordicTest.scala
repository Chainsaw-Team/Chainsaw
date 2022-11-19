package Chainsaw.dsp

import Chainsaw._
import breeze.numerics.constants._
import breeze.numerics.{cos, sin}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CordicTest extends AnyFlatSpec {

  val testCount = 1000
  val testIteration = 13
  val testFraction = 18

  // TODO: valid input for Hyper & Linear mode

  def getGroup: Seq[Double] = {
    def getOne: Double = Random.nextDouble() * 2 - 1 // [-1,1]

    val phase0 = getOne * Pi // [-Pi, Pi]
    val length = getOne.abs
    val phase1 = 0.0
    Seq(cos(phase0) * length, sin(phase0) * length, phase1)
  }

  val validGroups = Seq.fill(testCount)(getGroup).flatten

  behavior of "Cordic"

  def cordicMetric(epsilonX: Double, epsilonY: Double, epsilonZ: Double) = (yours: Seq[Double], golden: Seq[Double]) => {
    val Seq(yourX, yourY, yourZ) = yours
    val Seq(gX, gY, gZ) = golden
    (yourX - gX).abs <= epsilonX && (yourY - gY).abs <= epsilonY && (yourZ - gZ).abs <= epsilonZ
  }

  def test(alg: AlgebraicMode, rot: RotationMode): Unit = {
    it should s"work for ${alg.getClass.getSimpleName} + ${rot.getClass.getSimpleName} mode" in {
      val gen = Cordic(alg, rot, iteration = testIteration, fraction = testFraction)
      ChainsawTest(s"testCordic", gen, validGroups).doTest()
    }
  }

  //  val algebraicModes = Seq(CIRCULAR, HYPERBOLIC, LINEAR)
  val algebraicModes = Seq(CIRCULAR)
  val rotationModes = Seq(ROTATION, VECTORING)

  algebraicModes.foreach(alg =>
    rotationModes.foreach(rot => test(alg, rot))
  )
}

