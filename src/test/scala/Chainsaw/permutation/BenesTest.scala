package Chainsaw.permutation

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BenesTest extends AnyFlatSpec {

  val data = (0 until 8).toList
  (0 until 1000).foreach { _ =>
    val permutation = Permutation.random(8)
    assert(permutation.permuted.equals(Benes.doBenes(data, permutation)))
  }
}
