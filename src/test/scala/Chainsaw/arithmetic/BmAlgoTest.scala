package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.xilinx._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BmAlgoTest extends AnyFlatSpec {

  behavior of "big multiplication algorithm"

  val widths = Seq(96, 256, 377, 1024)
  val multTypes = Seq(FullMultiplier, SquareMultiplier) // FIXME: implement MsbMultiplier & LsbMultiplier mode
  widths.foreach(width =>
    multTypes.foreach { multType =>
      it should s"work for $width bit ${className(multType)}" in {
        val solution = BmSearch.getParetos(width, multType).head
        BmAlgo(solution).selfTest()
      }
    }
  )
}
