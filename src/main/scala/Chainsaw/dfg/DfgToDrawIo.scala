package Chainsaw.dfg

import java.io.{File, FileOutputStream}

/** methods visualizing Chainsaw Dfg by mapping it to a .drawio xml file
 *
 */
object DfgToDrawIo {

  def apply(dfg: Dfg, fileName: String): Unit = {

    import dfg._

    clarify()

    val prefix =
      """
        |<mxfile host="65bd71144e">
        |    <diagram name="page 1" id="EoULSVm_BbEfXEleqs-Q">
        |        <mxGraphModel dx="691" dy="1120" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="827" pageHeight="1169" math="0" shadow="0">
        |            <root>
        |                <mxCell id="0"/>
        |                <mxCell id="1" parent="0"/>""".stripMargin

    val postfix =
      """
        |            </root>
        |        </mxGraphModel>
        |    </diagram>
        |</mxfile>
        |""".stripMargin

    def vertexToXml(vertex: DfgVertex, x: Int, y: Int) = {
      val id    = vertex.hashCode()
      val label = vertex.name
      val style = "ellipse;whiteSpace=wrap;html=1;aspect=fixed;"
      s"""
         |<mxCell id="$id" value="$label" style="$style" parent="1" vertex="1">
         |    <mxGeometry x="$x" y="$y" width="40" height="40" as="geometry"/>
         |</mxCell>""".stripMargin
    }

    def edgeToXml(edge: DfgEdge) = {
      val id     = edge.hashCode()
      val label  = edge.delay.toString
      val style  = "edgeStyle=none;html=1;"
      val source = edge.source.hashCode()
      val target = edge.target.hashCode()
      s"""
         |<mxCell id="$id" value="$label" style="$style" edge="1" parent="1" source="$source" target="$target">
         |    <mxGeometry relative="1" as="geometry"/>
         |</mxCell>""".stripMargin
    }

    val indent = "    " * 4
    val content = (vertexSeq.map(vertexToXml(_, 0, 0)).mkString("") + edgeSeq.map(edgeToXml).mkString("")) // content
      .split("\n")
      .map(indent + _)
      .mkString("\n") // with indent

    val xml              = prefix + content + postfix
    val fileNameWithPostfix = s"$fileName.drawio"
    val file = new File(fileNameWithPostfix)
    val fileOutputStream = new FileOutputStream(fileNameWithPostfix)
    println(s"view your dfg at ${file.getAbsolutePath}")
    fileOutputStream.write(xml.getBytes)
    fileOutputStream.close()
  }

}
