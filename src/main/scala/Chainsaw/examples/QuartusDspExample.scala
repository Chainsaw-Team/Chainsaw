package Chainsaw.examples

import Chainsaw._
import Chainsaw.intel._
import spinal.core._

import scala.language.postfixOps

case class QuartusDspExample() extends Module {

  val a, b, c, d = in UInt (18 bits)
  val e, f       = out UInt (36 bits)
  e := (a * b).d()
  f := (c * d).d()

}

object QuartusDspExample {
  def main(args: Array[String]): Unit = {
    new QuartusFlow(QuartusDspExample()).impl()
  }
}
