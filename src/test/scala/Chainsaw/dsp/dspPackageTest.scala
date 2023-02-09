package Chainsaw.dsp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._       // for finite state machine dialect
import spinal.lib.bus._       // for all kinds of bus and regIf
import spinal.lib.bus.regif._ // for regIf
import spinal.sim._           // for simulation
import spinal.core.sim._      // for more simulation

class dspPackageTest extends org.scalatest.flatspec.AnyFlatSpec {

  "plot_spectrum" should "work" in {
    val signal =
      (0 until 10000).map(_.toDouble).map(scala.math.sin).map(BigDecimal(_))
    plot_spectrum(signal, 240 MHz)
  }

}
