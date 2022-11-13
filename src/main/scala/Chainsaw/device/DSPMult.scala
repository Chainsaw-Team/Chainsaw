package Chainsaw.device

import Chainsaw.device._
import spinal.lib._
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.language.postfixOps


class DSPMultFull(width: Int = 16) extends Component {
  val io = new Bundle {
    val a = in UInt (width bits)       // AH
    val b = in UInt (width bits)       // AL
    val c = in UInt (width bits)       // BH
    val d = in UInt (width bits)       // BL
    val Multiplier1 = out Bits (width*2 bits)
    val Multiplier2 = out Bits (width*2 bits)
    val ret = out UInt (width*4 bits)    // Mult result
    // FIXME: Just for test
    val Test_CPlusD                = out UInt(18 bits)
    val Test_DSP0ret               = out UInt(width*2 bits)
    val Test_DSP1ret               = out UInt(width*2 bits)
    val Test_DSP2ret               = out UInt(48 bits)
    val Test_DSP2retMinusBD        = out UInt(48 bits)
    val Test_DSP2retMinusBDShifted = out UInt(width*4 bits)
    val Test_ACconcatBD            = out Bits(width*4 bits)

  }
  noIoPrefix()

  // DSP0: a * c (width*2 bits), no add
  val AC_Mult = new DSP48E2Cell_MultAdd(0,1,0,1)
  AC_Mult.io.A := io.a.resize(30)
  AC_Mult.io.B := io.c.resize(18)
  AC_Mult.io.C := U(0)
  val DSP0ret = AC_Mult.io.P((width*2-1) downto 0)    // take low width*2 bits
  val DSP0ret_2p = RegNext(RegNext(DSP0ret).init(0)).init(0)


  // DSP1: b * d (width*2 bits), no add
  val BD_Mult = new DSP48E2Cell_MultAdd(1,2,0,1)
  BD_Mult.io.A := io.b.resize(30)
  BD_Mult.io.B := io.d.resize(18)
  BD_Mult.io.C := U(0)
  val DSP1ret = BD_Mult.io.P((width*2-1) downto 0)    // take low width*2 bits
  val DSP1ret_1p = RegNext(DSP1ret).init(0)

  // c + d
  val CPlusD = io.c +^ io.d    // width+1 bits. Attention: Safe addition must be used!

  // DSP2: (a + b) * (c + d) - (a * c)
  val ADCDAC = new DSP48E2Cell_AddMultMinus(2,1,0)
  ADCDAC.io.A := io.a.resize(30)
  ADCDAC.io.D := io.b.resize(27)
  ADCDAC.io.B := CPlusD.resize(18)
  ADCDAC.io.C := DSP0ret.resize(48)
  val DSP2ret = ADCDAC.io.P

  // (a + b) * (c + d) - (a * c) - (b * d)
  val DSP2retMinusBD = DSP2ret - DSP1ret_1p
  val DSP2retMinusBDShifted = DSP2retMinusBD << width

  // AC concat BD
  val ACConcatBD = DSP0ret_2p ## DSP1ret_1p    // {AC, BD}, (width*4) bits

  // (a * c) ## (b * d) + [(a + b) * (c + d) - (a * c) - (b * d)]
  io.ret := (RegNext(ACConcatBD.asUInt).init(0) + RegNext(DSP2retMinusBDShifted).init(0)).resize(width * 4)  // TODO: check this

  // FIXME: Just for test
  io.Multiplier1 := io.a ## io.b
  io.Multiplier2 := io.c ## io.d

  io.Test_CPlusD  := CPlusD.resize(18)
  io.Test_DSP0ret := DSP0ret
  io.Test_DSP1ret := DSP1ret
  io.Test_DSP2ret := DSP2ret
  io.Test_DSP2retMinusBD := DSP2retMinusBD
  io.Test_DSP2retMinusBDShifted := DSP2retMinusBDShifted.resized

  io.Test_ACconcatBD := ACConcatBD.asBits

}    // Done


// TODO: Make it work
class DSPMultKara(width: Int = 16) extends Component {
  val io = new Bundle {
    val a = in UInt (width bits)       // AH
    val b = in UInt (width bits)       // AL
    val c = in UInt (width bits)       // BH
    val d = in UInt (width bits)       // BL
    val Multiplier1 = out Bits (width*2 bits)
    val Multiplier2 = out Bits (width*2 bits)
    val ret = out UInt (width*4 bits)    // Mult result
    // FIXME: Just for test
    val Test_CPlusD                = out UInt(18 bits)
    val Test_DSP0ret               = out UInt(width*2 bits)
    val Test_DSP1ret               = out UInt(width*2 bits)
    val Test_DSP2ret               = out UInt(48 bits)
    val Test_DSP2retMinusBD        = out UInt(48 bits)
    val Test_DSP2retMinusBDShifted = out UInt(width*4 bits)
    val Test_ACconcatBD            = out Bits(width*4 bits)

  }
  noIoPrefix()

  // DSP0: a * c (width*2 bits), no add
  val AC_Mult = new DSP48E2Cell_MultAdd(0,1,0,1)
  AC_Mult.io.A := io.a.resize(30)
  AC_Mult.io.B := io.c.resize(18)
  AC_Mult.io.C := U(0)
  val DSP0ret = AC_Mult.io.P((width*2-1) downto 0)    // take low width*2 bits
  val DSP0ret_2p = RegNext(RegNext(DSP0ret).init(0)).init(0)


  // DSP1: b * d (width*2 bits), no add
  val BD_Mult = new DSP48E2Cell_MultAdd(1,2,0,1)
  BD_Mult.io.A := io.b.resize(30)
  BD_Mult.io.B := io.d.resize(18)
  BD_Mult.io.C := U(0)
  val DSP1ret = BD_Mult.io.P((width*2-1) downto 0)    // take low width*2 bits
  val DSP1ret_1p = RegNext(DSP1ret).init(0)

  // c + d
  val CPlusD = io.c +^ io.d    // width+1 bits. Attention: Safe addition must be used!

  // DSP2: (a + b) * (c + d) - (a * c)
  val ADCDAC = new DSP48E2Cell_AddMultMinus(2,1,0)
  ADCDAC.io.A := io.a.resize(30)
  ADCDAC.io.D := io.b.resize(27)
  ADCDAC.io.B := CPlusD.resize(18)
  ADCDAC.io.C := DSP0ret.resize(48)
  val DSP2ret = ADCDAC.io.P

  // (a + b) * (c + d) - (a * c) - (b * d)
  val DSP2retMinusBD = DSP2ret - DSP1ret_1p
  val DSP2retMinusBDShifted = DSP2retMinusBD << width

  // AC concat BD
  val ACConcatBD = DSP0ret_2p ## DSP1ret_1p    // {AC, BD}, (width*4) bits

  // (a * c) ## (b * d) + [(a + b) * (c + d) - (a * c) - (b * d)]
  io.ret := (RegNext(ACConcatBD.asUInt).init(0) + RegNext(DSP2retMinusBDShifted).init(0)).resize(width * 4)  // TODO: check this

  // FIXME: Just for test
  io.Multiplier1 := io.a ## io.b
  io.Multiplier2 := io.c ## io.d

  io.Test_CPlusD  := CPlusD.resize(18)
  io.Test_DSP0ret := DSP0ret
  io.Test_DSP1ret := DSP1ret
  io.Test_DSP2ret := DSP2ret
  io.Test_DSP2retMinusBD := DSP2retMinusBD
  io.Test_DSP2retMinusBDShifted := DSP2retMinusBDShifted.resized

  io.Test_ACconcatBD := ACConcatBD.asBits

}    // Done








object DSPMult_Main {
  def main(args: Array[String]) {
    NewSpinalConfig.generateVerilog(new DSPMultFull(16)).printRtl()
    //    NewSpinalConfig.generateVerilog(new DSP_Mult_Low).printRtl()
    //    NewSpinalConfig.generateVerilog(new DSP_Mult_Square).printRtl()
  }
}