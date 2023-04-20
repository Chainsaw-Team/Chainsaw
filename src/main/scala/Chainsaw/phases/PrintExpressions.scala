package Chainsaw.phases

import spinal.core._
import spinal.core.internals._

import scala.language.postfixOps

// FIXME: can't traverse MemWrite by current rec methods
/** An example of traversing all expressions and statements in the AST
  */
class PrintExpressions extends Phase {
  override def impl(pc: PhaseContext) = {

    recComponent(pc.topLevel)

    def recComponent(c: Component): Unit = {
      c.children.foreach(recComponent)
      c.dslBody.foreachStatements(recStatement)
    }

    def recStatement(s: Statement): Unit = {
      s.foreachExpression(recExpression)
      s match {
        case ts: TreeStatement => ts.foreachStatements(recStatement)
        case _                 =>
      }
      println(s.getClass)
    }

    def recExpression(e: Expression): Unit = {
      e.foreachExpression(recExpression)
      println(e.getClass)
    }
  }

  override def hasNetlistImpact = false

  override def toString = s"${super.toString}"
}

object PrintExpressions {
  def main(args: Array[String]): Unit = {
    val phase = new PrintExpressions

    println("example 1")
    SpinalConfig()
      .addTransformationPhase(phase)
      .generateVerilog(new Module {
        val a, b = in UInt (8 bits)
        val sel  = in Bool ()
        val c    = out UInt (8 bits)
        c := Mux(sel, a, b) // become binary multiplexer
      })

    println("example 2")
    SpinalConfig()
      .addTransformationPhase(phase)
      .generateVerilog(new Module {
        val a, b, c, d = in UInt (8 bits)
        val sel        = in UInt (2 bits)
        val e          = out UInt (8 bits)
        e := sel.muxList(Seq(0 -> a, 1 -> b, 2 -> c, 3 -> d)) // become switch statement
      })
  }
}
