package Chainsaw.dsp

import Chainsaw._
import testConfigurations._

class CordicTest extends ChainsawFlatSpec {

  val testIteration = 13
  val testFraction = 18
  val algebraicModes = Seq(CIRCULAR) // TODO: other algebraic modes
  val rotationModes = Seq(ROTATION, VECTORING)

  algebraicModes.foreach(alg =>
    rotationModes.foreach(rot =>
      testGenerator(Cordic(alg, rot, iteration = testIteration, fraction = testFraction), cordicSynth, cordicImpl)
    )
  )
}

