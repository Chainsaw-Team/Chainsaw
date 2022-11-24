package Chainsaw.arithmetic

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import testConfigurations._

class CpaTest extends ChainsawFlatSpec {

  behavior of "Cpa Mode Test"

  val adderTypes = Seq(BinaryAdder, BinarySubtractor, TernaryAdder, TernarySubtractor1, TernarySubtractor2)
  val cpaModes = Seq(M2M, M2S, S2M, S2S)
  val withCarrys = Seq(true, false)
  adderTypes.foreach { adderType =>
    cpaModes.foreach(cpaMode =>
      withCarrys.foreach(withCarry =>
        testGenerator(Cpa(adderType, Seq.fill(4)(10), cpaMode, withCarry), cpaSynth, cpaImpl)))
  }

}
