package Chainsaw.phases

import Chainsaw._
import spinal.core.GlobalData
import spinal.core.internals.{Phase, PhaseContext}

class DrawHierarchy extends Phase {
  override def impl(pc: PhaseContext): Unit = {
    val top = GlobalData.get.toplevel.asInstanceOf[ChainsawBaseModule]
    RetimingGraph(top).toComponentGraph
  }

  override def hasNetlistImpact: Boolean = false
}
