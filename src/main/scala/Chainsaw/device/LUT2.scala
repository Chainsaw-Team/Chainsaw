package Chainsaw.device
import spinal.core._

import scala.language.postfixOps

case class LUT2(init: BigInt) extends Unisim {
  val generic: Generic = new Generic {
    val INIT = B(init, 4 bits)
  }
  val I0, I1 = in Bool ()
  val O      = out Bool ()
  addPrimitive("LUT2")
}

object LUT2 {
  def process(input: Seq[Bool], init: BigInt): Seq[Bool] = {
    val lut = LUT2(init)
    lut.I0 := input(0)
    lut.I1 := input(1)
    Seq(lut.O)
  }
}
