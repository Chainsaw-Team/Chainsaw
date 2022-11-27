package Chainsaw

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Try
import org.apache.commons.io.FileUtils

class ChainsawFlatSpec extends AnyFlatSpec {

  def testGenerator(gen: => ChainsawGenerator, synth: Boolean = false, impl: Boolean = false): Unit = {

    behavior of gen.name

    it should "have a correct naive implementation" in {
      if (gen.implNaiveH.isDefined) {
        gen.setAsNaive()
        gen.doSelfTest()
      }
    }

    it should "have a correct implementation" in {
      naiveSet.clear()
      gen.doSelfTest()
    }

    if (synth)
      it should "meet the util requirement after synth" in
        ChainsawSynth(gen, s"synth_${gen.name}", withRequirement = true)

    if (impl)
      it should "meet the util requirement after impl" in
        ChainsawImpl(gen, s"impl_${gen.name}", withRequirement = true)
  }
}
