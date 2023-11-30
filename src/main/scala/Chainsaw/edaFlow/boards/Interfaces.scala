package Chainsaw.edaFlow.boards

import spinal.core._
import spinal.lib.IMasterSlave
import spinal.lib.Stream

// M2C = Mezzanine-to-Carrier,that is, sub-module to FPGA, vise versa
// CC = clock capable pins, these pins can be used for clock signals.
// FIXME: use "generate" instead of lazy

abstract class Fmc extends Bundle with IMasterSlave {

  lazy val CLK_M2C_P, CLK_M2C_N = Bits(2 bits)
  val LA_P, LA_N                = Bits(34 bits) // 68 single-ended signals/34 differential pairs, 00,01,17,18 are CC
  lazy val SCL, SDA             = Bool()        // I2C serial clock & data

  // these are GTH transceiver pins, set_property would failed on them
  val DP_M2C_P: Bits
  val DP_M2C_N: Bits
  val DP_C2M_P: Bits
  val DP_C2M_N: Bits

  def asCarrier(): Unit = { // to drive a carrier
    in(CLK_M2C_P, CLK_M2C_N, DP_M2C_N, DP_M2C_P)
    out(DP_C2M_P, DP_C2M_N)
  }

  def asMezzanine(): Unit = { // to drive a mezzanine
    out(CLK_M2C_P, CLK_M2C_N, DP_M2C_N, DP_M2C_P)
    in(DP_C2M_P, DP_C2M_N)
  }
}

/** low-pin-count FMC connector
  *
  * @see
  *   [[https://fmchub.github.io/appendix/VITA57_FMC_HPC_LPC_SIGNALS_AND_PINOUT.html]] for pin definition
  */
class FmcLpc() extends Fmc {

  lazy val DP_M2C_P, DP_M2C_N, DP_C2M_P, DP_C2M_N = Bits(1 bits) // multi-gigabit transceiver data pairs

  override def asMaster(): Unit = out(LA_P, LA_N)

}

object FmcLpc {
  def apply(): FmcLpc = new FmcLpc()
}

class FmcHpc() extends Fmc {
  lazy val DP_M2C_P, DP_M2C_N, DP_C2M_P, DP_C2M_N = Bits(10 bits) // multi-gigabit transceiver data pairs
  val HA_P, HA_N = Bits(24 bits) // 48 single-ended signals/24 differential pairs, 00,01,17,18 are CC
  val HB_P, HB_N = Bits(22 bits) // 44 single-ended signals/22 differential pairs, 00,06,17 are CC

  override def asMaster(): Unit = out(HA_P, HA_N, HB_P, HB_N, LA_P, LA_N)

}

class Pcie(pinCount: Int) extends Bundle with IMasterSlave {
  assert(Seq(1, 2, 4, 8, 16).contains(pinCount), "pinCount must be 1,2,4,8 or 16")
  val perstn, refclk = Bool()
  val rx             = Bits(pinCount bits)
  val tx             = Bits(pinCount bits)
  this.setName("pcie")

  override def asMaster(): Unit = {
    in(perstn, refclk, rx)
    out(tx)
  }

}

object Pcie {
  def apply(pinCount: Int): Pcie = new Pcie(pinCount)
}
