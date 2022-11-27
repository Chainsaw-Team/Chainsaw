package Chainsaw.dsp

import Chainsaw._
import Chainsaw.intel.QuartusFlow
import Chainsaw.testConfigurations.{filterImpl, filterSynth}

import scala.util.Random

class FirTest extends ChainsawFlatSpec {

  val filterPrecision = FilterPrecision(coeffType = SFixInfo(4, 12), dataType = SFixInfo(4, 12))

  // make sure that the latency is correct for all given parallel factors and lengths

  val coeffLengths = Seq(5, 11, 17)
  val coeffs = coeffLengths.map(Seq.fill(_)(Random.nextDouble()))
  val symmetrics = Seq(true, false)
  val symmetricCoeffs = coeffs.map(coeff => coeff ++ coeff.reverse)
  val parallelFactors = Seq(4)

  // Fir
  coeffs.foreach(coeff => testGenerator(Fir(coeff, filterPrecision), synth = filterSynth, impl = filterImpl))
  symmetricCoeffs.foreach(coeff => testGenerator(Fir(coeff, filterPrecision, symmetric = true), synth = filterSynth, impl = filterImpl))
  // Fir with parallel factors
  coeffs.foreach(coeff =>
    parallelFactors.foreach { parallelFactor =>
      testGenerator(ParallelFir(coeff, filterPrecision, parallelFactor), synth = filterSynth, impl = filterImpl)
    })

  behavior of "quartus flow"

  //  it should "work for intel device" in new QuartusFlow(ParallelFir(coeffs.last, filterPrecision, 2).implH).impl()

}
