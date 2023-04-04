package Chainsaw.phases

import Chainsaw._
import spinal.core.GlobalData
import spinal.core.internals._
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._
import spinal.lib.fsm._
import spinal.lib.bus._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TimingDrc extends Phase {

  override def impl(pc: PhaseContext) = {

    logger.info("start applying timing DRC")

    val trace = mutable.Queue[String]()

    recComponent(pc.topLevel)

    def recComponent(c: Component): Unit = {
      trace += c.toString()
      c.children.foreach(recComponent)
      c.dslBody.foreachStatements(recStatement)
      trace.dequeue()
    }

    def recStatement(s: Statement): Unit = {
      trace += s.toString
      s.foreachExpression(recExpression)
      s match {
        case ts: TreeStatement => ts.foreachStatements(recStatement)
        case _                 =>
      }
      trace.dequeue()
    }

    // TODO: find MUX, judge whether the result is registered or not
    def recExpression(e: Expression): Unit = {
      trace += e.toString()
      e match {
//        case op: Operator.BitVector.Add =>
//          if (op.left.getWidth > 27 || op.right.getWidth > 27)
//            println(s"Found ${op.left} + ${op.right}, ${op.left.getWidth} bits + ${op.right.getWidth} bits")
        case op: Operator.BitVector.Mul =>
          if (op.left.getWidth > 27 || op.right.getWidth > 27) {
            println(s"big mult: ${op.left.getWidth} bits * ${op.right.getWidth} bits -> $op")
            println(trace.zipWithIndex.map { case (str, i) => "\t" * i + str }.mkString("\n -> "))
          }
        case _ =>
      }
      e.foreachExpression(recExpression)
      trace.dequeue()
    }

  }

  override def hasNetlistImpact = false

  override def toString = s"${super.toString}"
}
