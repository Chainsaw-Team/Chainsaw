package Chainsaw.device

import spinal.core._
import spinal.lib.History
import scala.language.postfixOps

/** @param nameIdx
  *   Index for the specified dsp
  * @param InnerDelayAB
  *   1 for A1B1, 2 for A2B2
  * @param ExtDelayAB
  *   ExtraDelay for AB
  * @param ExtDelayC
  *   ExtraDelay for C
  */
// DSP48E2 Configuration for Mult-Add (A * B + C)
class DSP48E2Cell_MultAdd(nameIdx: Int, InnerDelayAB: Int = 1, ExtDelayAB: Int = 0, ExtDelayC: Int = 1)
    extends Component {
  val io = new Bundle {
    // Direct In Out signal
    val A             = in UInt (30 bits)
    val B             = in UInt (18 bits)
    val C             = in UInt (48 bits)
    val P             = out UInt (48 bits)
    val PATTERNDETECT = out Bool () // For Pattern Recognition
  }
  noIoPrefix()

  val attr = DSPAttrBuilder().setMult(MULTMODE.AB1).build // AB1 for A * B
  // TODO: pay attention here
  attr.AREG       = 2 // A1: AREG = 2 and INMODE[0] = 1, or AREG = 1 and INMODE[0] = 0;  A2: AREG = 2 and INMODE[0] = 0
  attr.BREG       = 2 // B1: BREG = 2 and INMODE[4] = 1, or BREG = 1 and INMODE[4] = 0;  B2: BREG = 2 and INMODE[4] = 0
  attr.ACASCREG   = 1
  attr.BCASCREG   = 1
  attr.DREG       = 1
  attr.ADREG      = 1
  attr.INMODEREG  = 1
  attr.ALUMODEREG = 1
  attr.OPMODEREG  = 1
  attr.CARRYINREG = 1
  attr.CARRYINSELREG = 1
  attr.PREG          = 1
  attr.AMULTSEL      = "A"
  attr.BMULTSEL      = "B"
  attr.PREADDINSEL   = "A"
  attr.USE_MULT      = "MULTIPLY"
  attr.MREG          = 1
  // TODO: for pattern recognition
  attr.MASK               = B"101111111111111111111111111111111111111111111111" // 48bits, 置1的bit被忽略
  attr.PATTERN            = B"010000000000000000000000000000000000000000000000" // 48bits, 想要匹配的模式
  attr.SEL_MASK           = "MASK"
  attr.SEL_PATTERN        = "PATTERN"
  attr.USE_PATTERN_DETECT = "PATDET"

  val dsp = DSP48E2(attr).setName(s"dsp_MultAdd_$nameIdx")
  dsp.RSTs.all.foreach(_.clear())
  dsp.CEs.all.foreach(_.set())

  // The delaying of input signals
  val delayedA = History(io.A, ExtDelayAB + 1, init = U(0, 30 bits)) // shift registers
  val lastA    = delayedA(delayedA.length - 1)

  val delayedB = History(io.B, ExtDelayAB + 1, init = U(0, 18 bits)) // shift registers
  val lastB    = delayedB(delayedB.length - 1)

  val delayedC = History(io.C, ExtDelayC + 1, init = U(0, 48 bits)) // shift registers
  val lastC    = delayedC(delayedC.length - 1)

  // ******** Mult-Add: A * B + C ******** //
  // direct input
  dsp.DATAIN.A       := lastA // link to the end of shift registers
  dsp.DATAIN.B       := lastB // link to the end of shift registers
  dsp.DATAIN.C       := lastC // link to the end of shift registers
  dsp.DATAIN.D       := U(0)
  dsp.DATAIN.CARRYIN := False // TODO: consider later

  // cascade input (all set as 0)
  dsp.CASCDATAIN.ACIN        := U(0)
  dsp.CASCDATAIN.BCIN        := U(0)
  dsp.CASCDATAIN.PCIN        := U(0)
  dsp.CASCDATAIN.CARRYCASCIN := False // TODO: consider later
  dsp.CASCDATAIN.MULTSIGNIN  := False // TODO: consider later

  // dynamic control signal
  dsp.INST.ALUMODE    := B"0000"
  dsp.INST.OPMODE     := B"110000101"
  dsp.INST.CARRYINSEL := B"000"

  //dsp.INST.INMODE := B"00000"    // for A2B2

  if (InnerDelayAB == 1) {
    dsp.INST.INMODE := B"10001" // for A1B1
  } else if (InnerDelayAB == 2) {
    dsp.INST.INMODE := B"00000" // for A2B2
  } else {
    dsp.INST.INMODE := B"10001"
  }

  // direct output
  io.P := dsp.DATAOUT.P

  // pattern detect output
  io.PATTERNDETECT := dsp.DATAOUT.PATTERNDETECT

}

// DSP48E2 Configuration for Add-Mult-Minus: (A + D) * B - C
class DSP48E2Cell_AddMultMinus(nameIdx: Int, ExtDelayABD: Int = 0, ExtDelayC: Int = 1) extends Component {
  val io = new Bundle {
    // Direct In Out signal
    val A = in UInt (30 bits)
    val B = in UInt (18 bits)
    val C = in UInt (48 bits)
    val D = in UInt (27 bits)
    val P = out UInt (48 bits)
  }
  noIoPrefix()

  val attr = DSPAttrBuilder().setMult(MULTMODE.AD0B1).build // AD0B1 for (A + D) * B
  // TODO: pay attention to here
  attr.AREG       = 1 // A1: AREG = 2 and INMODE[0] = 1, or AREG = 1 and INMODE[0] = 0;  A2: AREG = 2 and INMODE[0] = 0
  attr.BREG       = 2 // B1: BREG = 2 and INMODE[4] = 1, or BREG = 1 and INMODE[4] = 0;  B2: BREG = 2 and INMODE[4] = 0
  attr.ACASCREG   = 1
  attr.BCASCREG   = 1
  attr.DREG       = 1
  attr.ADREG      = 1 // for ADREG
  attr.INMODEREG  = 1
  attr.ALUMODEREG = 1
  attr.OPMODEREG  = 1
  attr.CARRYINREG = 1
  attr.CARRYINSELREG = 1
  attr.PREG          = 1
  attr.AMULTSEL      = "AD" // "A" or "AD"
  attr.BMULTSEL      = "B"  // "B" or "AD"
  attr.PREADDINSEL   = "A"  // "A" or "B"
  attr.USE_MULT      = "MULTIPLY"
  attr.MREG          = 1

  val dsp = DSP48E2(attr).setName(s"dsp_AddMultMinus_$nameIdx")
  dsp.RSTs.all.foreach(_.clear())
  dsp.CEs.all.foreach(_.set())

  // The delaying of input signals
  val delayedA = History(io.A, ExtDelayABD + 1, init = U(0, 30 bits)) // shift registers
  val lastA    = delayedA(delayedA.length - 1)

  val delayedB = History(io.B, ExtDelayABD + 1, init = U(0, 18 bits)) // shift registers
  val lastB    = delayedB(delayedB.length - 1)

  val delayedC = History(io.C, ExtDelayC + 1, init = U(0, 48 bits)) // shift registers
  val lastC    = delayedC(delayedC.length - 1)

  val delayedD = History(io.D, ExtDelayABD + 1, init = U(0, 27 bits)) // shift registers
  val lastD    = delayedD(delayedD.length - 1)

  // ******** Add-Mult-Minus: (A + D) * B - C ******** //
  // direct input
  dsp.DATAIN.A       := lastA // link to the end of shift registers
  dsp.DATAIN.B       := lastB // link to the end of shift registers
  dsp.DATAIN.C       := lastC // link to the end of shift registers
  dsp.DATAIN.D       := lastD // link to the end of shift registers
  dsp.DATAIN.CARRYIN := False //

  // cascade input (all set as 0)
  dsp.CASCDATAIN.ACIN        := U(0)
  dsp.CASCDATAIN.BCIN        := U(0)
  dsp.CASCDATAIN.PCIN        := U(0)
  dsp.CASCDATAIN.CARRYCASCIN := False
  dsp.CASCDATAIN.MULTSIGNIN  := False

  // dynamic control signal
  dsp.INST.ALUMODE    := B"0001"      // - Z + (W + X + Y + CIN) - 1
  dsp.INST.OPMODE     := B"000110101" // W=0, Z=C, X=Y=M
  dsp.INST.INMODE     := B"00100"     // for A1,D1,AD,B2
  dsp.INST.CARRYINSEL := B"110"       // Seems to work. Old one with DRC error is "001"

  // direct output
  io.P := dsp.DATAOUT.P

}
