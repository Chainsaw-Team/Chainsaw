package Chainsaw.crypto

import Chainsaw.{ChainsawFlatSpec, TestConfig}
import Chainsaw.project.zprize.ZPrizeMSM.baseModulus
import cc.redberry.rings
import cc.redberry.rings.scaladsl._

import scala.io.Source

class CryptoIpTests extends ChainsawFlatSpec {

  def testBarrett377(): Unit = {
    val gen = Barrett377()
    testOperator(gen, generatorConfigTable("Barrett377"))
  }

  override def algoNames = Seq("EllipticCurve", "BarrettFine")

  override val generatorConfigTable = Map(
    "Barrett377" -> TestConfig(
      full  = true,
      naive = true,
      synth = false,
      impl  = false
    )
  )

//  testBarrett377()
}
