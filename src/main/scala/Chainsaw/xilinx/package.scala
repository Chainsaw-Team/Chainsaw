package Chainsaw

import spinal.core._

import scala.language.postfixOps
import Chainsaw._
import Chainsaw.xilinx._
import Chainsaw.edaFlow._

package object xilinx {


  val xilinxCDConfig = ClockDomainConfig( // recommended by Xilinx UG901
    clockEdge              = RISING,
    resetKind              = ASYNC,
    resetActiveLevel       = HIGH,
    softResetActiveLevel   = HIGH,
    clockEnableActiveLevel = HIGH
  )

  // TODO: add Zybo, Nexys4 and their xdc files
  // TODO: add utils for the following boards and the attribute "budget" a must-be
  val vu9p = XilinxDevice(
    UltraScale,
    "xcvu9p-flga2104-2-i",
    600 MHz,
    None,
    budget = VivadoUtil(lut = 1182240, ff = 2364480, dsp = 6840, bram36 = 2160, uram288 = 960, carry8 = 147780)
  )
  val zcu104  = XilinxDevice(UltraScale, "xczu7ev-ffvc1156-2-e", 200 MHz, None)
  val u250    = XilinxDevice(UltraScale, "XCU250-FIGD2104-2L-E".toLowerCase, 600 MHz, None)
  val u200    = XilinxDevice(UltraScale, "XCU200-FSGD2104-2-E".toLowerCase, 600 MHz, None, budget = vu9p.budget)
  val kcu1500 = XilinxDevice(UltraScale, "xcku115-flvb2104-2-e", 800 MHz, None)
  val a7100t = XilinxDevice(
    Series7,
    "XC7A100T-CSG324-1".toLowerCase(),
    100 MHz,
    None,
    budget = VivadoUtil(lut = 32600, ff = 65200, dsp = 240)
  )
}
