package Chainsaw.coding

import Chainsaw._
import com.mxgraph.layout._
import com.mxgraph.util.mxCellRenderer
import org.jgrapht._
import org.jgrapht.ext.JGraphXAdapter
import org.jgrapht.graph._
import spinal.core._

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
case class ParityCheckMatrix(value: Seq[Seq[Int]]) {

  assert(value.flatten.forall(ele => ele == 1 || ele == 0), "parity check matrix contains 0s and 1s only")
  def messageNodeCount = value.head.length
  def checkNodeCount   = value.length
}
class TannerNode() {
  def neighbors(implicit tannerGraph: TannerGraph): mutable.Set[TannerNode] = {
    Graphs.neighborSetOf(tannerGraph, this).asScala
  }
}
class MessageNode(idx: Int) extends TannerNode() {
  override def toString = s"b$idx"
}
class CheckNode(idx: Int) extends TannerNode() {
  override def toString = s"c$idx"
}

class TannerEdge(
                  var messageLikelihood: Llr = Llr(0),
                  var checkLikelihood: Llr   = Llr(0)
) {
  def messageNode(implicit tannerGraph: TannerGraph) = tannerGraph.getEdgeSource(this).asInstanceOf[MessageNode]
  def checkNode(implicit tannerGraph: TannerGraph)   = tannerGraph.getEdgeTarget(this).asInstanceOf[CheckNode]

  override def toString = ""
}
case class TannerGraph() extends SimpleGraph[TannerNode, TannerEdge](classOf[TannerEdge]) {

  val messageNodes = ArrayBuffer[MessageNode]()
  val checkNodes   = ArrayBuffer[CheckNode]()
  val baseValues   = ArrayBuffer[Llr]()

  implicit val refGraph: TannerGraph = this

  def addMessageNode(idx: Int): MessageNode = {
    val node = new MessageNode(idx)
    addVertex(node)
    messageNodes += node
    node
  }

  def addCheckNode(idx: Int): CheckNode = {
    val node = new CheckNode(idx)
    addVertex(node)
    checkNodes += node
    node
  }

  def updateFromRx(received: Seq[Llr]) = {
    assert(received.length == messageNodes.length)
    received.foreach(baseValues += _)
    messageNodes.zip(received).foreach { case (message, likelihood) =>
      message.neighbors.foreach { check =>
        getEdge(message, check).messageLikelihood = likelihood
        println(s"L[$message -> $check] = ${likelihood.value}")
      }
    }
  }

  def updateCheckValues() = {
    checkNodes.foreach(check =>
      check.neighbors.foreach { message =>
        val likelihood = check.neighbors
          .filterNot(_ == message)
          .map(other => getEdge(other, check).messageLikelihood)
          .reduce(_ ⊞ _)
        getEdge(message, check).checkLikelihood = likelihood
        if(verbose >= 1) println(s"L[$check -> $message] = ${likelihood.value}")
      }
    )
  }

  def updateMessageValues(last: Boolean) = {
    messageNodes.zip(baseValues).foreach { case (message, base) =>
      message.neighbors.foreach { check =>
        val cond =
          if (last) (_: TannerNode) => false
          else (node: TannerNode) => node == check // for last iteration, take all sources into account

        val likelihood = message.neighbors
          .filterNot(cond)
          .map(other => this.getEdge(message, other).checkLikelihood)
          .reduce(_ + _) + base

        this.getEdge(message, check).messageLikelihood = likelihood
        if(verbose >= 1) println(s"L[$message -> $check] = ${likelihood.value}")
      }
    }
  }

  def beliefPropagation(received: Seq[Llr], iter: Int): Seq[Llr] = {
    assert(iter >= 1)
    updateFromRx(received)
    (0 until iter).foreach { i =>
      updateCheckValues()
      updateMessageValues(i == iter - 1)
    }
    messageNodes.map(message => this.edgesOf(message).asScala.head.messageLikelihood)
  }

  def saveFig(name: String) = {
    val graphAdapter = new JGraphXAdapter(this)         // manager which store the information for rendering
    val layout       = new mxCircleLayout(graphAdapter) // for tanner
    layout.execute(graphAdapter.getDefaultParent) // starts from a tree layout
    val image   = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
    val imgFile = new File(dagOutputDir, s"$name.png")
    logger.info(s"view the DFG of $name at ${imgFile.getAbsolutePath}")
    ImageIO.write(image, "PNG", imgFile)
  }
}

object TannerGraph {
  def apply(parityCheckMatrix: ParityCheckMatrix) = {
    val graph = new TannerGraph()
    // add nodes
    (0 until parityCheckMatrix.messageNodeCount).foreach(i => graph.addMessageNode(i))
    (0 until parityCheckMatrix.checkNodeCount).foreach(i   => graph.addCheckNode(i))
    if(verbose >= 1) println(graph.vertexSet().asScala.mkString(" "))
    // add edges
    parityCheckMatrix.value.zipWithIndex.foreach { case (row, checkId) =>
      row.zipWithIndex.foreach { case (ele, messageId) =>
        if (ele == 1) {
          val src = graph.messageNodes(messageId)
          val des = graph.checkNodes(checkId)
          graph.addEdge(src, des, new TannerEdge())
          if(verbose >= 1) println(s"add $src -> $des")
        }
      }
    }
    println(graph.edgeSet().size())
    graph
  }
}

object TannerGraphTest extends App {

  val parityCheckMatrix = ParityCheckMatrix(
    Seq(
      Seq(1, 1, 0, 0, 1, 0, 0, 0, 0),
      Seq(1, 0, 1, 0, 0, 0, 1, 0, 0),
      Seq(0, 1, 0, 1, 0, 0, 0, 1, 0),
      Seq(0, 0, 1, 1, 0, 1, 0, 0, 0),
      Seq(0, 0, 0, 0, 1, 1, 0, 0, 1),
      Seq(0, 0, 0, 0, 0, 0, 1, 1, 1)
    )
  )

  val graph = TannerGraph(parityCheckMatrix)
  graph.saveFig("tanner")
  val received = Seq(5.6, -10.2, 0.7, 0.5, -7.5, 12.2, 8.5, 6.9, -7.7).map(Llr(_))
  println(s"${graph.beliefPropagation(received, 2).map(_.decideBpsk)}")
}
