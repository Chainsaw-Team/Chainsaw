package Chainsaw.examples

import spinal.core._
import spinal.core.internals.{LeafStatement, StatementDoubleLinkedContainer, StatementDoubleLinkedContainerElement, SwitchStatement, TreeStatement, WhenStatement}

import scala.language.postfixOps

class MuxComparison(style: Int) extends Component {

  val a, b, c, d = in UInt (8 bits)
  val ctrl = in UInt (2 bits)
  val r = out UInt (8 bits)

  style match {
    case 0 =>
      when(ctrl === 0)(r := a)
        .elsewhen(ctrl === 1)(r := b)
        .elsewhen(ctrl === 2)(r := c)
        .otherwise(r := d)
    case 1 =>
      switch(ctrl) {
        is(0)(r := a)
        is(1)(r := b)
        is(2)(r := c)
        is(3)(r := d)
      }
    case 2 =>
      val mapping = Seq(0 -> a, 1 -> b, 2 -> c, 3 -> d)
      r := ctrl.muxList(mapping)
    case 3 =>
      val mid0 = Mux(ctrl(0), a, b)
      val mid1 = Mux(ctrl(0), c, d)
      r := Mux(ctrl(1), mid0, mid1)
    case 4 =>
      val vec = Vec(a, b, c, d)
      r := vec(ctrl)
  }

  val context: Unit = GlobalData.get.phaseContext.topLevel
    .dslBody.foreachStatements { s =>
    println(s"${ClassName(s)} -> $s")
    s match {
      case statement: TreeStatement => statement match {
        case statement: SwitchStatement =>
          statement.elements.foreach(println)
        case statement: WhenStatement =>
          println(statement.whenTrue)
          println(statement.whenFalse)
      }
      case _ =>
    }
  }

  setDefinitionName(s"MuxComparison_$style")
}

object MuxComparison {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new MuxComparison(0))
    //    SpinalVerilog(new MuxComparison(1))
    //    SpinalVerilog(new MuxComparison(2))
    //    SpinalVerilog(new MuxComparison(3))
    //    SpinalVerilog(new MuxComparison(4))
  }
}
