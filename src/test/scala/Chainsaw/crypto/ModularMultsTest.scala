package Chainsaw.crypto

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import Chainsaw.project.zprize.ZPrizeMSM.baseModulus

class ModularMultsTest extends AnyFlatSpec {

  behavior of "ModularMultsTest"

  val data = Seq.fill(100000)(BigInt(377, Random), BigInt(377, Random))

  it should "barrett" in BarrettAlgo(baseModulus).selfTest()

  it should "barrettFine" in BarrettFineAlgo(baseModulus).selfTest()

}
