package Chainsaw.phases

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.xilinx._

import spinal.core.internals._

/** padding for generators with unaligned data
 *
 */
class IoAlign extends Phase {
  override def impl(pc: PhaseContext): Unit = {

    def buf[T <: Data](that: T, cycle: Int) = KeepAttribute(that.d(cycle)).addAttribute("DONT_TOUCH")

    val component = pc.topLevel
    require(component.isInstanceOf[ChainsawBaseModule], "TimePad can only be applied to ChainsawBaseModule")
    val module = component.asInstanceOf[ChainsawBaseModule]
    require(module.gen.isInstanceOf[Unaligned])
    val gen = module.gen.asInstanceOf[Unaligned]

    require(gen.inputTimes.min == 0)

    module.rework {
      // TODO: pad flowIn and flowOut.npz correctly
      val newFlowIn = slave Flow Fragment(cloneOf(module.dataIn))
      module.validIn.setAsDirectionLess().allowDirectionLessIo
      module.lastIn.setAsDirectionLess().allowDirectionLessIo
      module.validIn := newFlowIn.valid
      module.lastIn := newFlowIn.last
      module.dataIn.zip(newFlowIn.fragment).zip(gen.inputTimes) foreach { case ((input, newInput), time) =>
        input.setAsDirectionLess().allowDirectionLessIo //allowDirectionLessIo is to disable the io Bundle linting
        newInput.setName(input.getName() + "_wrap")
        input := buf(newInput, time)
      }
      module.flowInPointer = newFlowIn
      module.flowIn.simPublic()
    }

    logger.info(s"[Progress] Chainsaw IoAlign phase done")
  }

  override def hasNetlistImpact = true
}
