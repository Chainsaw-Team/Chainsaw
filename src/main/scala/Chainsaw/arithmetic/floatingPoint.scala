package Chainsaw.arithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib.experimental.math._

case class Ieee2Flopoco32() extends BlackBox {
  val clk = in Bool ()
  mapClockDomain(clock = clk)
  val X = in Bits (32 bits)
  val R = out Bits (34 bits)
  setDefinitionName("InputIEEE_8_23_to_8_23")
  addRTLPath("src/main/resources/fp32/ieee2flopoco.v")
}

case class Flopoco2Ieee32() extends BlackBox {
  val clk = in Bool ()
  mapClockDomain(clock = clk)
  val X = in Bits (34 bits)
  val R = out Bits (32 bits)
  setDefinitionName("OutputIEEE_8_23_to_8_23")
  addRTLPath("src/main/resources/fp32/flopoco2ieee.v")
}

case class FlopocoAdd() extends BlackBox {
  val clk = in Bool ()
  mapClockDomain(clock = clk)
  val X, Y = in Bits (34 bits)
  val R    = out Bits (34 bits)
  setDefinitionName("FPAdd_8_23_F10_uid2")
  addRTLPath("src/main/resources/fp32/singlePrecisionAdd.v")
}

case class SinglePrecisionAdd() extends Module {

  val x, y = in(Floating32())
  val z    = out(Floating32())

  Seq(x, y, z).foreach(_.flattenForeach(_.simPublic()))

  val i2f0, i2f1 = Ieee2Flopoco32()
  val f2i        = Flopoco2Ieee32()
  val add        = FlopocoAdd()

  i2f0.X := x.asBits
  i2f1.X := y.asBits
  add.X  := i2f0.R
  add.Y  := i2f1.R
  f2i.X  := add.R
  z.assignFromBits(RegNext(f2i.R)) // latency = 1

}

object TestFps {
  def main(args: Array[String]): Unit = {
    val genConfig = SpinalConfig()
    SpinalSimConfig().withConfig(genConfig).withFstWave.compile(SinglePrecisionAdd()).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()

      (0 until 10).foreach { i =>
        dut.x #= 3.14f
        dut.y #= 5.58f
        dut.clockDomain.waitSampling()
        println(dut.z.toFloat)
      }
    }

  }
}
