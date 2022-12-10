package Chainsaw.phases

import Chainsaw._
import Chainsaw.dag.dagFigDir
import com.mxgraph.layout._
import com.mxgraph.util.mxCellRenderer
import org.jgrapht.ext.JGraphXAdapter
import org.jgrapht.graph.{DefaultEdge, DirectedWeightedMultigraph}
import spinal.core._
import spinal.core.internals._

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class DrawDfg() extends Phase {
  override def impl(pc: PhaseContext): Unit = {

    val graph = new DirectedWeightedMultigraph[ScalaLocated, DefaultEdge](classOf[DefaultEdge])

    val topIos = pc.topLevel.getAllIo
    val innerIos = pc.topLevel.children.flatMap(_.getAllIo)
    val allIOs = (topIos ++ innerIos).toSeq

    /** --------
     * add vertices
     -------- */
    topIos.foreach(graph.addVertex)
    pc.topLevel.children.foreach(graph.addVertex)

    /** --------
     * add edges
     -------- */
    def addEdge(target: BaseType, top: Boolean): Unit = {
      val (source, latency) = getSourceAndLatency(target)
      if (source != null) {
        logger.info(s"find edge from $source to $target, latency $latency")
        val targetVertex = if (top) target else target.getComponent()
        val sourceVertex = if (topIos.contains(source)) source else source.getComponent()
        val edge = graph.addEdge(sourceVertex, targetVertex)
        graph.setEdgeWeight(edge, latency)
      } else logger.info(s"no edge to $target")

      def getSourceAndLatency(to: BaseType): (BaseType, Int) = {
        val from = allIOs.filterNot(_ == to)
        val pendingQueues = Array.fill(2)(ArrayBuffer[Expression]()) // 2, as we do not take Memories into consideration
        var target = from.head.asInstanceOf[Expression]

        def walk(that: Expression): Boolean = {
          if (from.contains(that)) {
            target = that
            return true
          }

          that match {
            case _: Mem[_] => throw new Exception("not supported yet")
            case _: MemReadSync => throw new Exception("not supported yet")
            case _: MemReadWrite => throw new Exception("not supported yet")
            case _: MemReadAsync => throw new Exception("not supported yet")
            case that: BaseType =>
              def walkInputs(func: Expression => Unit): Unit = {
                that.foreachStatements(s => {
                  s.foreachDrivingExpression(input => {
                    func(input)
                  })
                  s.walkParentTreeStatementsUntilRootScope(tree => tree.walkDrivingExpressions(input => {
                    func(input)
                  }))
                })
              }

              if (that.isReg) {
                walkInputs(input => pendingQueues(1) += input)
                return false
              } else {
                walkInputs(input => if (walk(input)) return true)
              }
              return false
            case that: Expression => {
              that.foreachDrivingExpression(input => {
                if (walk(input)) return true
              })
              return false
            }
          }
        }

        var depth = 0
        pendingQueues(0) += to // init
        while (pendingQueues.exists(_.nonEmpty)) {
          // find sources whose distance to target <= 1
          pendingQueues(0).foreach(node => {
            if (walk(node)) return (target.asInstanceOf[BaseType], depth)
          })
          // all queues move forward for 1 cycle
          val temp = pendingQueues(0)
          for (i <- 0 until pendingQueues.length - 1) {
            pendingQueues(i) = pendingQueues(i + 1)
          }
          pendingQueues(pendingQueues.length - 1) = temp
          pendingQueues.last.clear()
          depth += 1
        }
        (null, -1)
      }
    }

    topIos.filter(_.isOutput).foreach(addEdge(_, top = true))
    innerIos.filter(_.isInput).foreach(addEdge(_, top = false))

    /** --------
     * draw
     -------- */
    val graphAdapter = new JGraphXAdapter[ScalaLocated, DefaultEdge](graph) // manager which store the information for rendering
    val layout = new mxCompactTreeLayout(graphAdapter, false, true)
    layout.setMoveTree(true)
    layout.setEdgeRouting(false)
    layout.setResizeParent(false)
    layout.setLevelDistance(5)

    layout.execute(graphAdapter.getDefaultParent) // starts from a tree layout
    val image = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
    val imgFile = new File(dagFigDir, s"${pc.topLevel.definitionName}.png")
    logger.info(s"view the DFG of ${pc.topLevel.definitionName} at ${imgFile.getAbsolutePath}")
    ImageIO.write(image, "PNG", imgFile)
  }

  override def hasNetlistImpact = false
}

object DrawDfg {

  case class Add(width: Int) extends Component {
    val io = new Bundle {
      val a, b = in UInt (width bits)
      val c = out UInt (width bits)
    }
    io.c := io.a + io.b
  }

  class Top extends Component {
    val a, b, c, d = in UInt (8 bits)
    val r = out UInt(8 bits)
    val core0, core1, core2 = Add(8)
    core0.io.a := a
    core0.io.b := b
    core1.io.a := c
    core1.io.b := d
    core2.io.a := core0.io.c
    core2.io.b := core1.io.c
    r := core2.io.c
  }

  def main(args: Array[String]): Unit = {
    val config = SpinalConfig()
    config.addTransformationPhase(new DrawDfg())
    //    config.generateVerilog(new SmallKara)
    config.generateVerilog(new Top)
  }
}
