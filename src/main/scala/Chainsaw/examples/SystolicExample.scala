package Chainsaw.examples

import Chainsaw.xilinx.VivadoSynth
import spinal.core._
import Chainsaw._
import Chainsaw.intel.QuartusFlow
import Chainsaw.xilinx._

import java.io.File
import scala.language.postfixOps
import scala.util.Random

// an example showing the performance of the a systolic array on FPGA
case class SystolicBlockBox() extends BlackBox {
  setDefinitionName("sfir_even_symmetric_systolic_top")
  val clk    = in Bool ()
  val datain = in SInt (16 bits)
  val firout = out SInt (32 bits)
  mapClockDomain(clock = clk)
  addRTLPath(
    "/home/ltr/Chainsaw/src/main/resources/netlists/sfir_even_symmetric_systolic_top.sv"
  )
}

case class SystolicExample() extends Component {
  setDefinitionName("sfir_even_symmetric_systolic_top")
  val datain = in SInt (16 bits)
  val firout = out SInt (32 bits)
  val core   = SystolicBlockBox()
  core.datain := datain.d()
  firout      := core.firout.d()
}

///** -------- part of the output of the synthesis report
//  * | Module Name                          | DSP Mapping                         | A Size | B Size | C Size | D Size | P Size | AREG | BREG | CREG | DREG | ADREG | MREG | PREG |
//  * |:-------------------------------------|:------------------------------------|:-------|:-------|:-------|:-------|:-------|:-----|:-----|:-----|:-----|:------|:-----|:-----|
//  * | sfir_even_symmetric_systolic_element | ((D'+A'')*(B:0x7)')'                | 16     | 16     | -      | 16     | 32     | 2    | 1    | -    | 1    | 1     | 1    | 1    |
//  * | sfir_even_symmetric_systolic_element | (PCIN+((D'+ACIN'')*(B:0xe)')')'     | 16     | 16     | -      | 16     | 32     | 2    | 1    | -    | 1    | 1     | 1    | 1    |
//  * | sfir_even_symmetric_systolic_element | (PCIN+((D'+ACIN'')*(B:0x3ff76)')')' | 16     | 16     | -      | 16     | 32     | 2    | 1    | -    | 1    | 1     | 1    | 1    |
//  * | sfir_even_symmetric_systolic_element | (PCIN+((D'+ACIN'')*(B:0x81)')')'    | 16     | 16     | -      | 16     | 32     | 2    | 1    | -    | 1    | 1     | 1    | 1    |
//  * --------
//  */

case class SystolicSpinal(bs: Seq[BigInt]) extends Component {
  val dataIn = in SInt (16 bits)
  val x      = dataIn.d(2)
  val xline  = Seq.iterate(x, bs.length)(_.d(2))
  val scaled =
    xline.zip(bs).map { case (port, b) => (port * S(b, 16 bits).d()).d() }
  // the first element is a dummy, it is a must for extreme fmax, or PREG won't be used for the first DSP
  val y = (S(0, 16 bits) +: scaled).reduce((a, b) => (a +^ b).d())
  out(y)
}

///** -------- part of the output of the synthesis report
//  * | Module Name   | DSP Mapping                   | A Size | B Size | C Size | D Size | P Size | AREG | BREG | CREG | DREG | ADREG | MREG | PREG |
//  * |:--------------|:------------------------------|:-------|:-------|:-------|:-------|:-------|:-----|:-----|:-----|:-----|:------|:-----|:-----|
//  * | synthSystolic | (A''*(B:0x15a8)')'            | 16     | 16     | -      | -      | 32     | 2    | 1    | -    | -    | -     | 1    | 1    |
//  * | synthSystolic | (PCIN+(ACIN''*(B:0xca06)')')' | 16     | 16     | -      | -      | 32     | 2    | 1    | -    | -    | -     | 1    | 1    |
//  * | synthSystolic | (PCIN+(ACIN''*(B:0xd37f)')')' | 16     | 16     | -      | -      | 32     | 2    | 1    | -    | -    | -     | 1    | 1    |
//  * | synthSystolic | (PCIN+(ACIN''*(B:0x8081)')')' | 16     | 16     | -      | -      | 32     | 2    | 1    | -    | -    | -     | 1    | 1    |
//  * | synthSystolic | (PCIN+(ACIN''*(B:0x962)')')'  | 16     | 16     | -      | -      | 32     | 2    | 1    | -    | -    | -     | 1    | 1    |
//  * --------
//  */

object SystolicExample extends App {
  val bs = Seq.fill(5)(BigInt(16, Random) - pow2(15))
  //  VivadoSynth(SystolicExample(), "synthXilinxSystolic")
  VivadoSynth(SystolicSpinal(bs), "synthOurSystolic")
  //  new QuartusFlow(SystolicSpinal(bs)).impl() // this architecture is also suitable for Intel Device(Cyclone V)
}
