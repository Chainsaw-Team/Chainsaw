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


/** For DSP48E2 */

object MULTMODE extends Enumeration {
  type MULTMODE = Value
  // postfix expression
  val AB1, AD0B1, AAD01, AD0AD01 = Value
}

import MULTMODE._

// ports for cascading
case class DSPCASC() extends Bundle {
  val A = UInt(30 bits)
  val B = UInt(18 bits)
  val P = UInt(48 bits)
  val CARRY = Bool()
  val MULTSIGN = Bool()
  val all = Seq(A, B, P, CARRY, MULTSIGN)
}

case class DSPCONTROL() extends Bundle { // 4
  val ALUMODE = Bits(4 bits)
  val INMODE = Bits(5 bits)
  val OPMODE = Bits(9 bits)
  val CARRYINSEL = Bits(3 bits)
}

case class DSPINPUT() extends Bundle { // 5
  // for a,b,c,d,special strategy is required when unused
  val A = in UInt (30 bits)
  val B = in UInt (18 bits)
  val C = in UInt (48 bits)
  val D = in UInt (27 bits)
  val CARRYIN = in Bool()
}

case class DSPOUTPUT() extends Bundle { // 7
  val P = out UInt (48 bits)
  val CARRYOUT = out UInt (4 bits)
  val XOROUT = out Bits (8 bits)
  val OVERFLOW, UNDERFLOW = out Bool()
  val PATTERNBDETECT, PATTERNDETECT = out Bool()
}

case class DSPCEs() extends Bundle { //
  val A1, A2, B1, B2, C, D, AD, M, P, CARRYIN, CTRL, INMODE, ALUMODE = Bool()
  val all = Seq(A1, A2, B1, B2, C, D, AD, M, P, CARRYIN, CTRL, INMODE, ALUMODE)
  all.foreach(signal => signal.setName("CE" + signal.getPartialName()))
}

case class DSPRSTs() extends Bundle {
  val A, B, C, D, M, P, ALLCARRYIN, CTRL, INMODE, ALUMODE = Bool()
  val all = Seq(A, B, C, D, M, P, ALLCARRYIN, CTRL, INMODE, ALUMODE)
  all.foreach(signal => signal.setName("RST" + signal.getPartialName()))
}


case class DSP48E2(attrs: DSPAttrs) extends Unisim { // This is actually a Blackbox

  addGenerics(attrs.generics: _*)

  val CLK = in Bool()
  // control
  val INST = in(DSPCONTROL())
  // inputs/outputs for cascading
  val CASCDATAIN = in(DSPCASC())
  val CASCDATAOUT = out(DSPCASC())
  // ClockEnables & ReSeTs
  val CEs = in(DSPCEs())
  val RSTs = in(DSPRSTs())
  // inputs/outputs from generic logic
  val DATAIN = in(DSPINPUT())
  val DATAOUT = out(DSPOUTPUT())

  // set names to be the same as primitive ports
  INST.setName("") // drop the prefix
  DATAIN.setName("")
  DATAOUT.setName("")

  CASCDATAIN.A.setName("ACIN")
  CASCDATAIN.B.setName("BCIN")
  CASCDATAIN.P.setName("PCIN")
  CASCDATAIN.CARRY.setName("CARRYCASCIN")
  CASCDATAIN.MULTSIGN.setName("MULTSIGNIN")

  CASCDATAOUT.A.setName("ACOUT")
  CASCDATAOUT.B.setName("BCOUT")
  CASCDATAOUT.P.setName("PCOUT")
  CASCDATAOUT.CARRY.setName("CARRYCASCOUT")
  CASCDATAOUT.MULTSIGN.setName("MULTSIGNOUT")

  val inputs = Seq(INST, CASCDATAIN, DATAIN, CEs, RSTs)
  val outputs = Seq(CASCDATAOUT, DATAOUT)

  mapClockDomain(clock = CLK)

  addPrimitive("DSP48E2")
}


class DSPAttrs() {

  // cascading strategy
  var A_INPUT = "DIRECT"
  var B_INPUT = "DIRECT"

  // pre-adder and mult
  var USE_MULT = "NONE" // "NONE", "MULTIPLY", "DYNAMIC"
  var AMULTSEL = "A"
  var BMULTSEL = "B"
  var PREADDINSEL = "A"
  var multMode = AB1

  // pipeline strategy
  // stage1 & 2
  var AREG, BREG, ACASCREG, BCASCREG = 1 // when set as 1, skip stage
  // stage1 for pipeline matching
  var DREG = 1
  var INMODEREG = 1
  // stage2 feed the pre-adder
  var ADREG = 1
  // stage3 feed the ALU
  var MREG, CREG = 1
  var CARRYINREG, CARRYINSELREG = 1
  var OPMODEREG, ALUMODEREG = 1
  // stage4
  var PREG = 1
  // TODO: @Victor Pattern Recognition:
  var MASK = B"111111111111111111111111111111111111111111111110" // 48bits, 置1的bit被忽略
  var PATTERN = B"000000000000000000000000000000000000000000000001" // 48bits, 想要匹配的模式
  var SEL_MASK = "MASK"
  var SEL_PATTERN = "PATTERN"
  var USE_PATTERN_DETECT = "PATDET"

  def generics = Seq(
    "A_INPUT" -> A_INPUT,
    "B_INPUT" -> B_INPUT,
    "AREG" -> AREG,
    "BREG" -> BREG,
    "ACASCREG" -> ACASCREG,
    "BCASCREG" -> BCASCREG,
    "DREG" -> DREG,
    "ADREG" -> ADREG,
    "INMODEREG" -> INMODEREG,
    "ALUMODEREG" -> ALUMODEREG,
    "OPMODEREG" -> OPMODEREG,
    "CARRYINREG" -> CARRYINREG,
    "CARRYINSELREG" -> CARRYINSELREG,
    "PREG" -> PREG,
    "AMULTSEL" -> AMULTSEL,
    "BMULTSEL" -> BMULTSEL,
    "PREADDINSEL" -> PREADDINSEL,
    "USE_MULT" -> USE_MULT,
    "MREG" -> MREG, // @ Victor: 我加的
    "MASK" -> MASK,
    "PATTERN" -> PATTERN,
    "SEL_MASK" -> SEL_MASK,
    "SEL_PATTERN" -> SEL_PATTERN,
    "USE_PATTERN_DETECT" -> USE_PATTERN_DETECT
  )
}


// builder of DSPAttrs
class DSPAttrBuilder {
  val ret = new DSPAttrs

  import ret._

  def setCascaded() = {
    A_INPUT = "CASCADE"
    B_INPUT = "CASCADE"
    this
  }

  def setMult(multMODE: MULTMODE): DSPAttrBuilder = {
    USE_MULT = "MULTIPLY"
    multMODE match {
      case AB1 => AMULTSEL = "A"; BMULTSEL = "B"; PREADDINSEL = "A";
      case AD0B1 => AMULTSEL = "AD"; BMULTSEL = "B"; PREADDINSEL = "A";
      case AAD01 => AMULTSEL = "A"; BMULTSEL = "AD"; PREADDINSEL = "A";
      case AD0AD01 => AMULTSEL = "AD"; BMULTSEL = "AD"; PREADDINSEL = "A";
    }
    multMode = multMODE
    this
  }

  def setALU() = {}

  // FIXME: following strategy is only available for MAC
  def setLatency(latency: Int) = {
    def setStage(stage: Int, set: Boolean): Unit = {
      val value = if (set) 1 else 0
      stage match {
        case 1 =>
          DREG = value
          INMODEREG = value
        case 2 =>
          ADREG = value
        case 3 =>
          MREG = value
          CREG = value
          CARRYINREG = value
          OPMODEREG = value
          ALUMODEREG = value
        case 4 =>
          PREG = value
      }
    }

    val sets = latency match {
      case 0 => Seq()
      case 1 => Seq(3)
      case 2 => Seq(3, 4)
      case 3 => Seq(2, 3, 4)
      case 4 => Seq(1, 2, 3, 4)
    }
    (1 to 4).foreach(i => if (sets.contains(i)) setStage(i, set = true) else setStage(i, set = false))

    latency match {
      case 4 => AREG = 2; ACASCREG = 2; BREG = 2; BCASCREG = 2;
      case 3 => AREG = 1; ACASCREG = 1; BREG = 1; BCASCREG = 1;
      case _ => AREG = 0; ACASCREG = 0; BREG = 0; BCASCREG = 0;
    }

    this
  }

  def build = ret
}


object DSPAttrBuilder {
  def apply(): DSPAttrBuilder = new DSPAttrBuilder()
}
