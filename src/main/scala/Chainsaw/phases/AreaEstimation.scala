package Chainsaw.phases

import Chainsaw.{ChainsawBaseModule, logger}
import spinal.core._
import spinal.core.internals._

import scala.collection.mutable.ArrayBuffer

/** print a hierarchy graph with estimated area for each sub component
  */
class AreaEstimation extends Phase {
  override def impl(pc: PhaseContext): Unit = {

    val lines = ArrayBuffer[String]()
    def dfs(current: ChainsawBaseModule, level: Int = 0): Unit = {
      lines += s"|${"  " * level}${current.name} -> ${current.gen.vivadoUtilEstimation}"
      val children = current.children
        .filter(_.isInstanceOf[ChainsawBaseModule])
        .map(_.asInstanceOf[ChainsawBaseModule])
      children.foreach(dfs(_, level + 1))
    }

    dfs(pc.topLevel.asInstanceOf[ChainsawBaseModule])

    logger.info(s"""
        |
        |----start area estimation----
        |${lines.mkString("\n")}
        |-----end area estimation-----
        |""".stripMargin)
  }
  override def hasNetlistImpact = false
}
