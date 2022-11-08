package Chainsaw.xilinx

import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw._

import java.io.File

class VivadoFlowTest extends AnyFlatSpec {

  behavior of "VivadoFlow"

  it should "doFlow" in {

    val targetDir = new File("synthWorkspace/synthSimple")
    targetDir.delete()

    DefaultVivadoFlow.synth(ChainsawDuts.simpleDut(true).implH, "synthSimple")

    assert(new File("synthWorkspace/synthSimple/synthSimple_after_synth.dcp").exists())

    DefaultVivadoFlow.impl(ChainsawDuts.simpleDut(true).implH, "synthSimple")

    assert(new File("synthWorkspace/synthSimple/synthSimple_after_place.dcp").exists())
    assert(new File("synthWorkspace/synthSimple/synthSimple_after_route.dcp").exists())
  }

}
