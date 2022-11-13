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


class DSPMultLow(width: Int = 16) extends Component {
  val io = new Bundle {
    val AH  = in  UInt(width bits)      // AH
    val AL  = in  UInt(width bits)      // AL
    val BH  = in  UInt(width bits)      // BH
    val BL  = in  UInt(width bits)      // BL
    val MultiplierA = out Bits (width*2 bits)
    val MultiplierB = out Bits (width*2 bits)
    val ret = out UInt(width*2 bits)    // low bit Mult
    // FIXME: Just for test
    val DSP0retLowBit_test  = out UInt(width bits)
    val DSP1retLowBit_test  = out UInt(width bits)
    val DSP1retShifted_test = out UInt(width*2 bits)
  }
  noIoPrefix()

  // DSP0: AL * BH (take low width bits), no add
  val ALBH_Mult = new DSP48E2Cell_MultAdd(0,1,0,1)    // 配平
  ALBH_Mult.io.A := io.AL.resize(30)
  ALBH_Mult.io.B := io.BH.resize(18)
  ALBH_Mult.io.C := U(0)
  val DSP0retLowBit = ALBH_Mult.io.P(width-1 downto 0)    // take low width bits

  // DSP1: AH * BL (take low width bits) + result of DSP0
  val AHBL_MultAdd = new DSP48E2Cell_MultAdd(1,2,1,0)
  AHBL_MultAdd.io.A := io.AH.resize(30)
  AHBL_MultAdd.io.B := io.BL.resize(18)
  AHBL_MultAdd.io.C := DSP0retLowBit.resize(48)
  val DSP1retLowBit = AHBL_MultAdd.io.P(width-1 downto 0)    // take low width bits
  val DSP1retShifted = DSP1retLowBit << width

  // DSP2: AL * BL (full mult) + result of DSP1
  val ALBL_MultAdd = new DSP48E2Cell_MultAdd(2,2,3,0)
  ALBL_MultAdd.io.A := io.AL.resize(30)
  ALBL_MultAdd.io.B := io.BL.resize(18)
  ALBL_MultAdd.io.C := DSP1retShifted.resize(48)
  val DSP2ret = ALBL_MultAdd.io.P(width*2-1 downto 0)    // take low width*2 bits

  io.ret := DSP2ret

  // FIXME: Just for test
  io.MultiplierA := io.AH ## io.AL
  io.MultiplierB := io.BH ## io.BL
  io.DSP0retLowBit_test := DSP0retLowBit
  io.DSP1retLowBit_test := DSP1retLowBit
  io.DSP1retShifted_test := DSP1retShifted

}    // Done


class DSPMultHigh(width: Int = 16) extends Component {
  val io = new Bundle {
    val AH  = in  UInt(width bits)      // AH
    val AL  = in  UInt(width bits)      // AL
    val BH  = in  UInt(width bits)      // BH
    val BL  = in  UInt(width bits)      // BL
    val MultiplierA = out Bits (width*2 bits)
    val MultiplierB = out Bits (width*2 bits)
    val ret = out UInt(width*2+1 bits)    // high bit Mult
    // FIXME: Just for test
    val DSP0retLowBit_test  = out UInt(48 bits)
    val DSP1retHighBit_test = out UInt(width+1 bits)
  }
  noIoPrefix()

  // DSP0: AL * BH, no add
  val ALBH_Mult = new DSP48E2Cell_MultAdd(0,1,0,1)    // 配平
  ALBH_Mult.io.A := io.AL.resize(30)
  ALBH_Mult.io.B := io.BH.resize(18)
  ALBH_Mult.io.C := U(0)
  val DSP0ret = ALBH_Mult.io.P

  // DSP1: AH * BL + result of DSP0 (take high width bits)
  val AHBL_MultAdd = new DSP48E2Cell_MultAdd(1,2,1,0)
  AHBL_MultAdd.io.A := io.AH.resize(30)
  AHBL_MultAdd.io.B := io.BL.resize(18)
  AHBL_MultAdd.io.C := DSP0ret
  val DSP1retHighBit = AHBL_MultAdd.io.P(2*width downto width)    // take width+1 bits

  // DSP2: AH * BH (full mult) + result of DSP1
  val AHBH_MultAdd = new DSP48E2Cell_MultAdd(2,2,3,0)
  AHBH_MultAdd.io.A := io.AH.resize(30)
  AHBH_MultAdd.io.B := io.BH.resize(18)
  AHBH_MultAdd.io.C := DSP1retHighBit.resize(48)
  val DSP2ret = AHBH_MultAdd.io.P(width*2 downto 0)    // take low width*2 bits

  io.ret := DSP2ret

  // FIXME: Just for test
  io.MultiplierA := io.AH ## io.AL
  io.MultiplierB := io.BH ## io.BL
  io.DSP0retLowBit_test := DSP0ret
  io.DSP1retHighBit_test := DSP1retHighBit

}    // Done


class DSPMultKara(widthA: Int = 16, widthB: Int = 25) extends Component {
  val io = new Bundle {
    val AH = in UInt (widthA bits)       // AH
    val AL = in UInt (widthA bits)       // AL
    val BH = in UInt (widthB bits)       // BH
    val BL = in UInt (widthB bits)       // BL
    val AHBH     = out UInt (widthA+widthB bits)
    val AHBLALBH = out UInt (widthA+widthB+1 bits)
    val ALBL     = out UInt (widthA+widthB bits)
    // FIXME: Just for test
    val MultiplierA = out Bits (widthA*2 bits)
    val MultiplierB = out Bits (widthB*2 bits)
  }
  noIoPrefix()

  // DSP0: AH * BH (widthA+widthB bits), no add
  val AHBH_Mult = new DSP48E2Cell_MultAdd(0,1,0,1)
  AHBH_Mult.io.A := io.BH.resize(30)
  AHBH_Mult.io.B := io.AH.resize(18)
  AHBH_Mult.io.C := U(0)
  val DSP0ret = AHBH_Mult.io.P((widthA+widthB-1) downto 0)    // take low widthA+widthB bits
  //  val DSP0ret_2p = RegNext(RegNext(DSP0ret).init(0)).init(0)    // FIXME: use me or not?

  // DSP1: AL * BL (widthA+widthB bits), no add
  val ALBL_Mult = new DSP48E2Cell_MultAdd(1,2,0,1)
  ALBL_Mult.io.A := io.BL.resize(30)
  ALBL_Mult.io.B := io.AL.resize(18)
  ALBL_Mult.io.C := U(0)
  val DSP1ret = ALBL_Mult.io.P((widthA+widthB-1) downto 0)    // take low widthA+widthB bits
  val DSP1ret_1p = RegNext(DSP1ret).init(0)

  // AH + AL
  val AHPlusAL = io.AH +^ io.AL    // width+1 bits. Attention: Safe addition must be used!

  // DSP2: (BH + BL) * (AH + AL) - (AH * BH)
  val BHBLAHALAHBH = new DSP48E2Cell_AddMultMinus(2,1,0)
  BHBLAHALAHBH.io.A := io.BH.resize(30)
  BHBLAHALAHBH.io.D := io.BL.resize(27)
  BHBLAHALAHBH.io.B := AHPlusAL.resize(18)
  BHBLAHALAHBH.io.C := DSP0ret.resize(48)
  val DSP2ret = BHBLAHALAHBH.io.P(widthA+widthB downto 0)    // take low widthA+widthB+1 bits

  // (BH + BL) * (AH + AL) - (AH * BH) - (AL * BL)
  val DSP2retMinusBD = DSP2ret - DSP1ret_1p.resize(widthA+widthB+1)

  // output
  io.AHBH := DSP0ret
  io.ALBL := DSP1ret
  io.AHBLALBH := DSP2retMinusBD

  // FIXME: Just for test
  io.MultiplierA := io.AH ## io.AL
  io.MultiplierB := io.BH ## io.BL

}    // Done


object DSPMult_Main {
  def main(args: Array[String]) {
//    NewSpinalConfig.generateVerilog(new DSPMultKara(16,25)).printRtl()
//    NewSpinalConfig.generateVerilog(new DSPMultFull(16)).printRtl()
//    NewSpinalConfig.generateVerilog(new DSPMultLow(16)).printRtl()
    NewSpinalConfig.generateVerilog(new DSPMultHigh(16)).printRtl()
//    NewSpinalConfig.generateVerilog(new DSP_Mult_Square).printRtl()
  }
}