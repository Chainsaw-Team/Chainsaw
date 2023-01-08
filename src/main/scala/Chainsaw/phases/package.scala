package Chainsaw

import spinal.core.internals._
import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

package object phases {

  /** using a phase on a generator
   */
  def testPhase(phase: Phase, gen: ChainsawBaseGenerator): Unit = {
    val config = SpinalConfig()
    config.addTransformationPhase(phase)
    config.generateVerilog(gen.implH)
  }

  // more netlist walkers for SpinalHDL
  implicit class ExpressionUtils(expression: Expression) {

    /** do func driving expressions for a given expression, compared with foreachDrivingExpression,
     * this method allows you to "go through" a driving statement, which may cut the path
     *
     * @param func the function to be applied on each driving expression
     */
    def foreachDriver(func: Expression => Unit): Unit = {
      expression match {
        case memPortStatement: MemPortStatement =>
          memPortStatement.foreachDrivingExpression(func)
          memPortStatement.mem.foreachStatements(_.foreachDrivingExpression(func))
        case baseType: BaseType =>
          baseType.foreachStatements(_.foreachDrivingExpression(func))
          baseType.foreachStatements(_.walkParentTreeStatementsUntilRootScope(_.foreachDrivingExpression(func)))
        case _ => expression.foreachDrivingExpression(func)
      }
    }

    /** find driving expressions for a given expression
     *
     * @return
     */
    def drivers: mutable.Seq[Expression] = {
      val buf = ArrayBuffer[Expression]()
      foreachDriver(buf += _)
      buf
    }

    /** a generalized method to walk driving expressions and apply a function
     *
     * @param func       the function to be applied on each driving expression
     * @param iter       determine driving expressions of the current expression
     * @param inc        determined the increment on depth after walking through a driving expression
     * @param end        extra condition to stop the walk earlier
     * @param leavesOnly when true, func will only execute on the leaves of the driving tree
     * @tparam T return type of T
     * @return whenever func executed, a return value will be added to the result buffer
     */
    def genericWalk[T](func: (Expression, Double) => T,
                       iter: Expression => Seq[Expression] = (e: Expression) => e.drivers,
                       inc: Expression => Double = (e: Expression) => 0.0,
                       end: (Expression, Double) => Boolean = (e: Expression, depth: Double) => false,
                       leavesOnly: Boolean = false): Seq[T] = {
      val visited = mutable.HashSet[Expression]()
      val buf = ArrayBuffer[T]()

      def dfs(expression: Expression, depth: Double = 0.0): Unit = {
        if (!visited.contains(expression)) {
          visited += expression
          val drivers = iter(expression)
          val isLeaf = drivers.isEmpty || end(expression, depth) // leaf is an expression with no driving expression, or end condition is met
          if (!leavesOnly || isLeaf) buf += func(expression, depth)
          if (!isLeaf) {
            val depthNext = inc(expression) + depth
            drivers.foreach(dfs(_, depthNext))
          }
        }
      }

      expression.drivers.foreach(dfs(_, 0.0))
      buf
    }

    def getSourcesAndDepths(filter: Expression => Boolean,
                            inc: Expression => Double) = {
      val sourcesAndWeights = mutable.HashMap[Expression, Double]()
      expression.genericWalk(
        func = (e: Expression, depth: Double) => if (filter(e)) sourcesAndWeights += e -> depth,
        end = (e: Expression, depth: Double) => filter(e),
        inc = inc,
        leavesOnly = true)
      sourcesAndWeights
    }

    def showDrivingPaths = {
      val iter = (e: Expression) => e.drivers
      val inc = (e: Expression) => 1.0
      val func = (e: Expression, acc: Double) => {
        println("\t" * acc.toInt + e.toString())
        e
      }
      val end = (e: Expression, acc: Double) => false
      genericWalk(func, iter, inc, end, leavesOnly = false)
    }

    def getAllPathsFrom(source: Expression): mutable.Seq[Seq[Expression]] = {
      val visited = mutable.HashSet[Expression]()
      val paths = mutable.ArrayBuffer[Seq[Expression]]()

      def dfs(expression: Expression, path: Seq[Expression]): Unit = {
        if (!visited.contains(expression)) {
          visited += expression
          if (expression == source) paths += path.reverse
          val drivers = expression.drivers
          drivers.foreach(e => dfs(e, path :+ e))
          visited -= expression
        }
      }

      dfs(expression, Seq(expression))
      paths
    }
  }

  implicit class BaseTypeUtil(base: BaseType) {

    // FIXME: this modified the netlist correctly, but destroy the validity of neighboring relationship stored in DoubleLinkedContainer
    def insertDelayBefore(cycle: Int, top: Component): Unit = {
      top.rework {
        val driver = base.getSingleDriver.get
        base.removeAssignments()
        base := driver.d(cycle)
        base
      }
    }
  }
}
