package Chainsaw.crypto

import Chainsaw.ChainsawFlatSpec
import Chainsaw.project.zprize.ZPrizeMSM.baseModulus
import cc.redberry.rings
import cc.redberry.rings.scaladsl._

import scala.io.Source

class CryptoIpTests extends ChainsawFlatSpec {

  val src = Source.fromFile("/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/BLS12377POINTS")
  val lines = src.getLines().toSeq

  // get points from file
  val points: Seq[EcPointAffine] = lines.map { line =>
    val Seq(xline, yline) = line.split(" ").toSeq
    val Seq(x, y) = Seq(xline, yline).map(str => BigInt(str.drop(2), 16))
    EcPointAffine(x, y)
  }

  val bls377 = new EllipticCurve(baseModulus, 0, 0, 0, 1)

  "elliptic curve" should "work with naive implementation for BLS-377" in bls377.selfTest(points)

  val betterBls377 = EllipticCurve(baseModulus, 0, 0, 0, 1)

  "short weierstrass curve" should "work with homo coordinates algorithm for BLS-377" in betterBls377.selfTest(points)

  behavior of "ModularMultsTest"

  it should "barrett" in BarrettAlgo(baseModulus).selfTest()

  it should "barrettFine" in BarrettFineAlgo(baseModulus).selfTest()

}
