package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.internals._

import scala.language.postfixOps
import Chainsaw.phases._
import Chainsaw._
import Chainsaw.xilinx._

case class DriverExample(exampleId: Int) extends Component {

  val a, b, c, d = in UInt (8 bits)
  val addr = in UInt (2 bits)
  val ctrl0, ctrl1 = in Bool()
  val r = out UInt (8 bits)

  exampleId match {
    case 0 =>
      val mem = Mem(UInt(8 bits), 4)
      mem.write(addr, a, enable = ctrl0)
      mem.write(addr, b, enable = ctrl1)
      r := mem.readSync(addr)
      r.showDrivingPaths // show the driving paths of r
    case 1 =>
      val aNext = a.d()
      aNext.setName("aNext")
      val bNext = b.d()
      bNext.setName("bNext")
      val cNext = c.d()
      cNext.setName("cNext")
      val dNext = d.d()
      dNext.setName("dNext")
      val mid0 = (aNext + bNext).d()
      mid0.setName("mid0")
      val mid1 = (cNext + dNext).d()
      mid1.setName("mid1")
      val mid2 = (mid0 + mid1).d()
      mid2.setName("mid2")
      r := mid2

      def end = (e: Expression, depth: Double) => (e.isInstanceOf[BaseType] && e.asInstanceOf[BaseType].isReg)

      def func = (e: Expression, depth: Double) => e

      def getPreviousRegs(e: Expression) = {
        e.genericWalk(func = func, end = end, leavesOnly = true)
      }

      val driversRegs = r.genericWalk(func = func, iter = getPreviousRegs)
      val driversAll = r.genericWalk(func = func)
      driversAll.foreach(println)
      println("-----")
      driversRegs.foreach(println)

      def inc = (e: Expression) => e match {
        case baseType: BaseType if baseType.isReg => 1.0
        case _ => 0.0
      }

      def returnLatency = (e: Expression, latency: Double) => (e, latency)

      def findMid0 = (e: Expression, latency: Double) => e == mid0

      r.genericWalk(func = returnLatency, inc = inc, leavesOnly = true).foreach(println)
      r.genericWalk(func = returnLatency, inc = inc).foreach(println)
    case 2 =>
      // 3 paths from a to r
      val mid = a * b
      r := ((a + (a << 4)).resize(8 bits) + mid).resize(8 bits)
      r.getAllPathsFrom(a).map(_.mkString(" -> ")).foreach(println)
  }


}

object DriverExample {
  def main(args: Array[String]): Unit = {
    //    SpinalVerilog(DriverExample(0))
    //    SpinalVerilog(DriverExample(1))
    SpinalVerilog(DriverExample(2))
  }
}
