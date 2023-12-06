package Chainsaw.examples

import spinal.core._
import spinal.core.sim._

import java.io.{File, PrintWriter}

case class DoubleAdd() extends BlackBox {

  // shortreal is not supported in Verilator
  val verilog =
    """
      |module DoubleAdd(
      |  input  [63:0] x,
      |  input  [63:0] y,
      |  output reg [63:0] z
      |  );
      |
      |  real x_real, y_real, z_real;
      |
      |  always @(*) begin
      |   z = $realtobits($bitstoreal(x) + $bitstoreal(y));
      |  end
      |
      |endmodule
      |""".stripMargin

  val rtlFile = new File("DoubleAdd.v")
  if (!rtlFile.exists()) {
    val rtlWriter = new PrintWriter(rtlFile)
    rtlWriter.write(verilog)
    rtlWriter.close()
  }
  addRTLPath(rtlFile.getAbsolutePath)

  val x, y = in(Bits(64 bits))
  val z    = out(Bits(64 bits))

}

object DoubleAdd extends App {
//  SpinalConfig().generateVerilog(DoubleAdd())
  SimConfig.compile(DoubleAdd()).doSim { dut =>
    dut.x #= java.lang.Double.doubleToLongBits(1.0)
    dut.y #= java.lang.Double.doubleToLongBits(1.0)
    sleep(10)
    println(java.lang.Double.longBitsToDouble(dut.z.toBigInt.toLong))
  }
}
