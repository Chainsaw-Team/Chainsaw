package Chainsaw.crypto

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import Chainsaw.project.zprize.ZPrizeMSM.baseModulus

class ModularMultsTest extends AnyFlatSpec {

  behavior of "ModularMultsTest"
  
  it should "barrett" in BarrettAlgo(baseModulus).selfTest()

  it should "barrettFine" in BarrettFineAlgo(baseModulus).selfTest()



}
