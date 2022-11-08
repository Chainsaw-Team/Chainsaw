package Chainsaw.device

import spinal.core._
import Chainsaw._
import java.io.File

import scala.language.postfixOps

/** Primitives in unisim library, usePrimitives in Chainsaw package should be set as true for synth, false for sim
 *
 * @see [[https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/Introduction]] for primitive definitions
 *
 */
class Unisim extends BlackBox {
  /** add the RTL path of a Unisim primitive, the Unisim source code is under control, this should be explicitly called in every subclass of Unisim
   *
   */
  def addPrimitive(name: String) = if (atSimTime) addRTLPath(new File(unisimDir, s"$name.v").getAbsolutePath)
}

/**
 * @see [[https://docs.xilinx.com/v/u/en-US/ug574-ultrascale-clb]] Carry Chain Primitive
 * @see ''Parhami, Behrooz. “Computer arithmetic - algorithms and hardware designs.” (2010).'' 5.6 MANCHESTER CARRY CHAINS AND ADDERS
 */
case class CARRY8() extends Unisim {
  val generic: Generic = new Generic {
    val CARRY_TYPE = "SINGLE_CY8"
  }
  val CO, O = out UInt (8 bits)
  val CI, CI_TOP = in Bool (1 bits)
  val DI, S = in UInt (8 bits) // S is the "propagate" input
  addPrimitive("CARRY8")
}

case class LUT6_2(init: BigInt) extends Unisim {
  val generic: Generic = new Generic {
    val INIT = B(init, 64 bits)
  }
  val I0, I1, I2, I3, I4, I5 = in Bool()
  val O5, O6 = out Bool()
  addPrimitive("LUT6_2")
}

case class DSP48E2(init: BigInt) extends Unisim {
  addPrimitive("DSP48E2.v")
}