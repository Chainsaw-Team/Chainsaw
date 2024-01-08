package Chainsaw.dfg

import org.graphstream.graph.implementations.MultiGraph
import org.graphstream.ui.view.Viewer

import org.graphstream.graph.implementations.SingleGraph
import org.graphstream.stream.file.FileSinkImages

import org.graphstream.graph.implementations.SingleGraph
import org.graphstream.stream.file.FileSinkImages
import org.graphstream.stream.file.FileSinkImages.{LayoutPolicy, OutputType, Resolutions}

import org.graphstream.graph.implementations.SingleGraph
import org.graphstream.stream.file.FileSinkImages
import org.graphstream.stream.file.FileSinkImages.{LayoutPolicy, OutputType, Resolutions}

import org.graphstream.graph.implementations.SingleGraph
import org.graphstream.ui.spriteManager.SpriteManager
import org.graphstream.stream.file.FileSinkImages
import org.graphstream.stream.file.FileSinkImages.{LayoutPolicy, OutputType, Resolutions}

object Main {
  def main(args: Array[String]): Unit = {
    System.setProperty("org.graphstream.ui", "swing")

    val graph = new SingleGraph("Tutorial")
    val sman = new SpriteManager(graph)

    graph.addNode("A")
    graph.addNode("B")
    graph.addNode("C")

    graph.addEdge("AB", "A", "B")
    graph.addEdge("BC", "B", "C")
    graph.addEdge("CA", "C", "A")

    val pic = new FileSinkImages(OutputType.PNG, Resolutions.VGA)
    pic.setLayoutPolicy(LayoutPolicy.COMPUTED_FULLY_AT_NEW_IMAGE)

    try {
      pic.writeAll(graph, "graph.png")
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}