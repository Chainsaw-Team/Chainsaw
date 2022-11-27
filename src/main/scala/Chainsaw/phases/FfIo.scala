package Chainsaw.phases

import spinal.core._
import spinal.core.internals._
import spinal.lib._

class FfIo extends Phase {
  override def impl(pc: PhaseContext): Unit = {
    //    def buf1[T <: Data](that: T) = KeepAttribute(RegNext(that)).addAttribute("DONT_TOUCH")

    def buf[T <: Data](that: T) = KeepAttribute(RegNext(that)).addAttribute("DONT_TOUCH")

    val component = pc.topLevel
    component.rework {
      val ios = component.getAllIo.toList
      ios.foreach { io =>
        if (io.getName() == "clk") {
          //Do nothing
        } else if (io.isInput) {
          io.setAsDirectionLess().allowDirectionLessIo //allowDirectionLessIo is to disable the io Bundle linting
          io := buf(in(cloneOf(io).setName(io.getName() + "_wrap")))
        } else if (io.isOutput) {
          io.setAsDirectionLess().allowDirectionLessIo
          out(cloneOf(io).setName(io.getName() + "_wrap")) := buf(io)
        } else ???
      }
    }
  }

  override def hasNetlistImpact = true
}

