//package Chainsaw.crypto
//
//import Chainsaw._
//import testConfigurations._
//import org.scalatest.flatspec.AnyFlatSpec
//
//import scala.util.Random
//
//class FineReductionTest extends ChainsawFlatSpec {
//
//  val Ms = Seq.fill(2)(BigInt(376, Random) + BigInt(1) << 376)
//  val upperBounds = Seq(2, 4, 6)
//
//  Ms.foreach(M =>
//    upperBounds.foreach(upperBound =>
//      testGenerator(FineReduction(M, upperBound), fineReductionSynth, fineReductionImpl)))
//
//  // FIXME: *** stack smashing detected ***: <unknown> terminated when using verilator
//}
