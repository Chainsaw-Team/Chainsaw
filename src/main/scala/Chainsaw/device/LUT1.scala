package Chainsaw.device
import spinal.core._

import scala.language.postfixOps

case class LUT1(init: BigInt) extends Unisim {
  val generic: Generic = new Generic {
    val INIT = B(init, 2 bits)
  }
  val I0 = in Bool ()
  val O  = out Bool ()
  addPrimitive("LUT1")
}

object LUT1 {
  def process(input: Seq[Bool], init: BigInt): Seq[Bool] = {
    val lut = LUT1(init)
    lut.I0 := input(0)
    Seq(lut.O)
  }
}
