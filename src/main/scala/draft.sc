import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus._
import spinal.lib.bus.regif._
import spinal.sim._
import spinal.core.sim._


val a = Seq(6,16,8,2)
a.scan(0)(_ + _).init