package Chainsaw

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Try

class ChainsawFlatSpec extends AnyFlatSpec {

  def testGenerator(gen: ChainsawGenerator, synth: Boolean = false, impl: Boolean = false): Unit = {

    behavior of gen.name

    atSimTime = true

    it should "have a correct naive implementation" in {
      gen.setAsNaive()
      gen.selfTest()
      naiveSet.clear()
    }

    it should "have a correct implementation" in gen.selfTest()

    if (synth)
      it should "meet the util requirement after synth" in
        ChainsawSynth(gen, s"synth_${gen.name}", withRequirement = true)

    if (impl)
      it should "meet the util requirement after impl" in
        ChainsawImpl(gen, s"impl_${gen.name}", withRequirement = true)
  }
}
