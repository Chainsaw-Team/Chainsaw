package Chainsaw.primitives
import spinal.core._

case class IBUFDS() extends BlackBox {
  val I  = in Bool ()
  val IB = in Bool ()
  val O  = out Bool ()
}

object IBUFDS {
  /** convert LVDS clock pair into single-ended clock
   */
  def Lvds2Clk(p: Bool, n: Bool): Bool = {
    val buf = IBUFDS()
    buf.I  := p
    buf.IB := n
    buf.O
  }
}
