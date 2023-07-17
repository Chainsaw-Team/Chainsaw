package Chainsaw.dfg

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.traverse._

import java.io._
import java.net._
import java.util._
import scala.collection.JavaConverters._

object GraphExample extends App {

  val g = new DefaultDirectedGraph[URI, DefaultEdge](classOf[DefaultEdge])

  val google    = new URI("http://www.google.com")
  val wikipedia = new URI("http://www.wikipedia.org")
  val jgrapht   = new URI("http://www.jgrapht.org")

  // add the vertices
  g.addVertex(google)
  g.addVertex(wikipedia)
  g.addVertex(jgrapht)

  // add edges to create linking structure
  g.addEdge(jgrapht, wikipedia)
  g.addEdge(google, jgrapht)
  g.addEdge(google, wikipedia)
  g.addEdge(wikipedia, google)

  val iter = new DepthFirstIterator(g)
  iter.asScala.foreach(println)

}
