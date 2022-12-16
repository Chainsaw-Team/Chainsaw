package Chainsaw.deprecated

import Chainsaw._
import Chainsaw.arithmetic.{BmAlgo, BmSolution, MultSearch}
import org.scalatest.flatspec.AnyFlatSpec

class BmAlgoTest extends AnyFlatSpec {

  behavior of "big multiplication algorithm"

  val widths = Seq(96, 256, 377, 1024)
  val multTypes = Seq(FullMultiplier, SquareMultiplier) // FIXME: implement MsbMultiplier & LsbMultiplier mode
  widths.foreach(width =>
    multTypes.foreach { multType =>
      it should s"work for $width bit ${className(multType)}" in {
        val solution = MultSearch.getBmParetos(width, multType).head
        BmAlgo(solution.asInstanceOf[BmSolution]).selfTest()
      }
    }
  )
}
