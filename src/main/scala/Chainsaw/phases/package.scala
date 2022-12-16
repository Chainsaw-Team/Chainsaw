package Chainsaw

import Chainsaw.project.das.DasSignalPro
import spinal.core.internals.Expression
import spinal.core.{BaseType, MemPortStatement, SpinalConfig}

import scala.collection.mutable
import spinal.core.internals._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

package object phases {

  def testPhase(phase: Phase, gen: ChainsawBaseGenerator): Unit = {
    val config = SpinalConfig()
    config.addTransformationPhase(phase)
    config.generateVerilog(gen.implH)
  }

  /** --------
   * utils for internal traversing
   * -------- */
  def getAllExpressionBetween(inputs: Seq[Expression], outputs: Seq[Expression]) = {
    val visited = mutable.HashSet[Expression]()
    visited ++= inputs

    def dfs(current: Expression): Unit = {
      if (!visited.contains(current)) {
        visited += current
        current.walkDrivers(dfs)
      }
    }

    outputs.foreach(dfs)
    visited
  }

  def getAllExpressionsInModule(top: ChainsawBaseModule) =
    getAllExpressionBetween(top.dataIn.map(_.raw), top.dataOut.map(_.raw))

  implicit class ExpressionUtils(expression: Expression) {

    /** recursively get all the expressions that drive this expression
     */
    def walkDrivers(func: Expression => Unit): Unit = {
      // three conditions for synchronous digital circuit
      expression match { // 1. RAM/ROM
        case that: MemPortStatement =>
          that.foreachDrivingExpression(func)
          that.mem.foreachStatements(s => s.foreachDrivingExpression(func))
        case that: BaseType => // 2. signal/register
          that.foreachStatements(s => {
            s.foreachDrivingExpression(func)
            s.walkParentTreeStatementsUntilRootScope(_.walkDrivingExpressions(func))
          })
        case that: Expression => that.foreachDrivingExpression(func) // 3. operator
      }
    }

    def getSourcesAndWeights(filter: Expression => Boolean,
                             eval: Expression => Double) = {
      val sourcesAndWeights = mutable.HashMap[Expression, Double]()
      val visited = mutable.HashSet[Expression]()

      def dfs(current: Expression, acc: Double = 0.0): Unit = {
        val weightNext = eval(current) + acc
        visited += current
        if (filter(current)) sourcesAndWeights += current -> weightNext
        else current.walkDrivers(e => if (!visited.contains(e)) dfs(e, weightNext))
      }

      expression.walkDrivers(dfs(_))
      sourcesAndWeights
    }

    def getUniqueDriver: BaseType = {
      def filter(expression: Expression) = expression.isInstanceOf[BaseType]
      def eval(expression: Expression) = 0.0
      val cands = getSourcesAndWeights(filter, eval)
      require(cands.size == 1)
      cands.keys.head.asInstanceOf[BaseType]
    }
  }

  implicit class BaseTypeUtil(base: BaseType) {

    def insertDelayBefore(cycle: Int, top:Component): Unit = {
      top.rework {
        val driver = cloneOf(base)
        driver assignFromBits base.getUniqueDriver.asBits
        base.removeAssignments()
        base := driver.d(cycle)
      }
    }
  }
}
