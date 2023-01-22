package Chainsaw.crypto

import Chainsaw._
import Chainsaw.project.zprize.ZPrizeMSM.baseModulus
import cc.redberry.rings.scaladsl._

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.ArrayBuffer

case class ShortWeierstrassCurve(
    override val modulus: IntZ,
    override val c: IntZ,
    override val d: IntZ
) extends EllipticCurve(modulus, 0, 0, c, d) {

  implicit val ecc: ShortWeierstrassCurve = this

  /** algorithm for padd & pdbl on homogeneous projective coordinate, 12M
    * required
    *
    * @see
    *   ''Complete addition formulas for prime order elliptic curves, Joost
    *   Renes, Craig Costello, Lejla Batina'' for the algorithm
    * @see
    *   ''PipeMSM: Hardware Acceleration for Multi-Scalar Multiplication,
    *   Charles. F. Xavier'' [[https://eprint.iacr.org/2022/999.pdf]] for the
    *   pipeline
    */
  def paddExtendedHomo(p0: EcPointProj, p1: EcPointProj): EcPointProj = {
    val zp = Zp(modulus)
    val (x1, y1, z1, t1, x2, y2, z2, t2) = (
      p0.axis(0),
      p0.axis(1),
      p0.axis(2),
      p0.axis(3),
      p1.axis(0),
      p1.axis(1),
      p1.axis(2),
      p1.axis(3)
    )

    // parallel version with a well-designed pipeline
    // stage0

    val r0 = zp.multiply(z1, z2)
    val r1 = zp.multiply(
      asBigInteger(
        BigInt(
          "0196bab03169a4f2ca0b7670ae65fc7437786998c1a32d217f165b2fe0b32139735d947870e3d3e4e02c125684d6e016",
          16
        )
      ),
      t2
    )

    val R1 = zp.subtract(y1, x1)
    val R2 = zp.subtract(y2, x2)
    val R3 = zp.add(y1, x1)
    val R4 = zp.add(y2, x2)

    val R5 = zp.multiply(R1, R2)
    val R6 = zp.multiply(R3, R4)
    val R7 = zp.multiply(t1, r1)
    val R8 = zp.multiply(2, r0)

    val R9  = zp.subtract(R6, R5)
    val R10 = zp.subtract(R8, R7)
    val R11 = zp.add(R8, R7)
    val R12 = zp.add(R6, R5)

    val X = zp.multiply(R9, R10)
    val Y = zp.multiply(R11, R12)
    val Z = zp.multiply(R10, R11)
    val T = zp.multiply(R9, R12)

    EcPointProj(X, Y, Z, T)(extendedHomo)

  }

  def sqrt(a: BigInt, p: BigInt): (IntZ, Boolean) = {
    if (p % 2 != 1 || a.modPow((p - 1) / 2, p) != 1) {
      return (asBigInteger(0), false)
    }

    def getST(a: BigInt): (BigInt, Int) = {
      //2 ^ t * s = a - 1
      var s = a - 1
      var t = 0
      while (s % 2 != 1) {
        s >>= 1
        t += 1
      }
      (s, t)
    }

    def getB(p: BigInt): BigInt = {
      var b = BigInt(1)
      while (b.modPow((p - 1) / 2, p) == 1) {
        b += 1
      }
      b
    }

    val (s, t) = getST(p)
    var x      = a.modPow((s + 1) / 2, p)
    var w      = a.modPow(s, p)
    val b      = getB(p)
    for (i <- 0 until t - 1) {
      if (w.modPow(BigInt(1) << (t - 2 - i), p) != 1) {
        val lambda = b.modPow((BigInt(1) << i) * s, p).mod(p)
        x = (x * lambda).mod(p)
        w = (w * lambda * lambda).mod(p)
      }
    }

    (asBigInteger(x), true)
  }

  object transformParameters {
    val zp = Zp(modulus)

    val a = zp.negativeOne
    val d = zp.divideExact(
      asBigInteger(
        BigInt(
          "0196bab03169a4f2ca0b7670ae65fc7437786998c1a32d217f165b2fe0b32139735d947870e3d3e4e02c125684d6e016",
          16
        )
      ),
      2
    )

    val A = zp.divideExact(zp.multiply(2, zp.add(a, d)), zp.subtract(a, d))
    val B = zp.divideExact(asBigInteger(4), zp.subtract(a, d))

    val xWeight = zp.negate(zp.divideExact(A, zp.multiply(3, B)))
    val (yWeight, valid) = sqrt(
      zp.divideExact(
        zp.subtract(zp.multiply(2, A, A, A), zp.multiply(9, A)),
        zp.multiply(27, B, B, B)
      ).toBigInt,
      baseModulus
    )
    require(valid)
  }

  //only supported for c = 0, d = 1
  def toTwistedEdwards(input: EcPointAffine): EcPointAffine = {
    val x = zp.multiply(input.x, transformParameters.xWeight)
    val y = zp.multiply(input.y, transformParameters.yWeight)

    val xMont = zp.subtract(
      zp.multiply(transformParameters.B, x),
      zp.divideExact(transformParameters.A, 3)
    )
    val yMont = zp.multiply(transformParameters.B, y)

    EcPointAffine(
      zp.divideExact(xMont, yMont),
      zp.divideExact(zp.subtract(xMont, 1), zp.add(xMont, 1))
    )
  }

  def fromTwistedEdwards(input: EcPointAffine): EcPointAffine = {
    val x = input.x
    val y = input.y

    val xMont = zp.divideExact(zp.add(1, y), zp.subtract(1, y))
    val yMont = zp.divideExact(xMont, x)

    EcPointAffine(
      zp.divideExact(
        zp.add(
          zp.divideExact(xMont, transformParameters.B),
          zp.divideExact(
            transformParameters.A,
            zp.multiply(3, transformParameters.B)
          )
        ),
        transformParameters.xWeight
      ),
      zp.divideExact(
        zp.divideExact(yMont, transformParameters.B),
        transformParameters.yWeight
      )
    )
  }

  // for homogeneous projective coordinates which use complete formula, pdbl is the same as padd
  //def pdblExtendedHomo(p: EcPointProj) = paddExtendedHomo(p, p)

  // TODO: better implementation of ladder on ShortWeierstrassCurve

  override def padd(p0: EcPointAffine, p1: EcPointAffine) = {
    val ret = fromTwistedEdwards(
      paddExtendedHomo(
        toTwistedEdwards(p0).toProjective(extendedHomo),
        toTwistedEdwards(p1).toProjective(extendedHomo)
      ).toAffine
    )
    assert(
      isOnCurve(ret) && ret == super.padd(p0, p1),
      s"padd failed on $p0 + $p1"
    )
    ret
  }

  //  override def paddGraph(width: Int): Dag = new Dag {
  //    val X1, Y1, Z1, T1, X2, Y2, Z2, T2 = InputVertex(UIntInfo(width))
  //    val X, Y, Z, T = OutputVertex(UIntInfo(width))
  //
  //    X1.vertex.setName("X1")
  //    Y1.vertex.setName("Y1")
  //    Z1.vertex.setName("Z1")
  //    T1.vertex.setName("T1")
  //    X2.vertex.setName("X2")
  //    Y2.vertex.setName("Y2")
  //    Z2.vertex.setName("Z2")
  //    T2.vertex.setName("T2")
  //    X.vertex.setName("X")
  //    Y.vertex.setName("Y")
  //    Z.vertex.setName("Z")
  //    T.vertex.setName("T")
  //
  //    val r0Mul = FakeMult(width).asVertex
  //    val r1CMul = FakeCMult(width, BigInt("0196bab03169a4f2ca0b7670ae65fc7437786998c1a32d217f165b2fe0b32139735d947870e3d3e4e02c125684d6e016", 16)).asVertex
  //
  //    val R1Sub = FakeAdd(width, true).asVertex
  //    val R2Sub = FakeAdd(width, true).asVertex
  //    val R3Add = FakeAdd(width, false).asVertex
  //    val R4Add = FakeAdd(width, false).asVertex
  //
  //    val R5Mul = FakeMult(width).asVertex
  //    val R6Mul = FakeMult(width).asVertex
  //    val R7Mul = FakeMult(width).asVertex
  //    val R8CMul = FakeCMult(width, BigInt(2)).asVertex
  //
  //    val R9Sub = FakeAdd(width, true).asVertex
  //    val R10Sub = FakeAdd(width, true).asVertex
  //    val R11Add = FakeAdd(width, false).asVertex
  //    val R12Add = FakeAdd(width, false).asVertex
  //
  //    val XMul = FakeMult(width).asVertex
  //    val YMul = FakeMult(width).asVertex
  //    val ZMul = FakeMult(width).asVertex
  //    val TMul = FakeMult(width).asVertex
  //
  //    r0Mul := (Z1, Z2)
  //    r1CMul := T2
  //
  //    R1Sub := (Y1, X1)
  //    R2Sub := (Y2, X2)
  //    R3Add := (Y1, X1)
  //    R4Add := (Y2, X2)
  //
  //    R5Mul := (R1Sub.out(0), R2Sub.out(0))
  //    R6Mul := (R3Add.out(0), R4Add.out(0))
  //    R7Mul := (T1, r1CMul.out(0))
  //    R8CMul := r0Mul.out(0)
  //
  //    R9Sub := (R6Mul.out(0), R5Mul.out(0))
  //    R10Sub := (R8CMul.out(0), R7Mul.out(0))
  //    R11Add := (R8CMul.out(0), R7Mul.out(0))
  //    R12Add := (R6Mul.out(0), R5Mul.out(0))
  //
  //    XMul := (R9Sub.out(0), R10Sub.out(0))
  //    YMul := (R11Add.out(0), R12Add.out(0))
  //    ZMul := (R10Sub.out(0), R11Add.out(0))
  //    TMul := (R9Sub.out(0), R12Add.out(0))
  //
  //    X := XMul.out(0)
  //    Y := YMul.out(0)
  //    Z := ZMul.out(0)
  //    T := TMul.out(0)
  //
  //    graphDone()
  //
  //    override def name: String = "padd"
  //
  //    /** --------
  //     * golden model
  //     * -------- */
  //    override def impl(dataIn: Seq[Any]): Seq[Any] = null
  //  }

  def pladder(
      XQP: IntZ,
      XRP: IntZ,
      M: IntZ,
      YP: IntZ
  ): (IntZ, IntZ, IntZ, IntZ) = {
    val zp = Zp(modulus)

    val YRBar    = zp.add(YP, zp.multiply(2, M, XRP))
    val E        = zp.subtract(XQP, XRP)
    val F        = zp.multiply(YRBar, E)
    val G        = zp.multiply(E, E)
    val XRPDot   = zp.multiply(XRP, G)
    val H        = zp.multiply(YRBar, YRBar)
    val MDot     = zp.multiply(M, F)
    val YPDot    = zp.multiply(YP, F, G)
    val K        = zp.add(H, MDot)
    val L        = zp.add(K, MDot)
    val MDotDot  = zp.subtract(XRPDot, K)
    val XSP      = zp.multiply(H, L)
    val XTP      = zp.add(zp.multiply(XRPDot, XRPDot), YPDot)
    val YPDotDot = zp.multiply(YPDot, H)

    (XSP, XTP, MDotDot, YPDotDot)
  }

  override def pmult(k: IntZ, p: EcPointAffine): EcPointAffine = {
    val zp = Zp(modulus)

    val Z2        = zp.multiply(4, p.y, p.y)
    var M         = zp.add(zp.multiply(3, p.x, p.x), c)
    var XQP: IntZ = 0
    var XRP       = zp.subtract(zp.multiply(M, M), zp.multiply(3, p.x, Z2))
    var YP        = zp.multiply(Z2, Z2)

    k.toBigInt.toString(2).tail.foreach { bit =>
      val pair =
        if (bit.asDigit == 1) pladder(XQP, XRP, M, YP)
        else pladder(XRP, XQP, M, YP)
      XQP = if (bit.asDigit == 1) pair._1 else pair._2
      XRP = if (bit.asDigit == 1) pair._2 else pair._1
      M   = pair._3
      YP  = pair._4
    }

    val XP =
      zp.divideExact(zp.subtract(zp.subtract(zp.multiply(M, M), XQP), XRP), 3)
    val Z = zp.divideExact(
      zp.divideExact(zp.multiply(p.x, YP), 2),
      zp.multiply(p.y, XP)
    )

    val ret = EcPointProj(
      zp.add(XP, XQP),
      zp.divideExact(zp.add(YP, zp.multiply(2, M, XQP)), 2),
      Z
    )(Jacobi).toAffine
    assert(isOnCurve(ret) && ret == multBase(k, p), s"pmult failed on $k * $p")
    ret
  }

}

//object ShortWeierstrassCurve extends App {
//  val curve = new ShortWeierstrassCurve(baseModulus, 0, 1)
//  val graph = curve.paddGraph(377)
//  toPng(graph, "padd")
//
//  def toPng(dag: Dag, pngName: String = null): Unit = {
//
//    implicit val refDag: Dag = dag
//
//    type V = DagVertex
//    type E = DagEdge
//    type Port = DagPort
//
//    import _root_.com.mxgraph.layout._
//    import _root_.com.mxgraph.util.mxCellRenderer
//    import org.jgrapht.ext.JGraphXAdapter
//
//    val graphAdapter = new JGraphXAdapter[V, E](dag) // manager which store the information for rendering
//
//    val vertexMap = graphAdapter.getVertexToCellMap
//    val edgeMap = graphAdapter.getEdgeToCellMap
//
//    // set styles of vertices/edges
//    def colorVertices(vertices: Seq[V], color: String): Array[AnyRef] = {
//      val targets = vertices.map(vertexMap.get(_)).toArray
//      graphAdapter.setCellStyle(s"fillColor=#$color", targets.asInstanceOf[Array[Object]])
//    }
//
//    def colorEdges(edges: Seq[E], color: String): Array[AnyRef] = {
//      val targets = edges.map(edgeMap.get(_)).toArray
//      graphAdapter.setCellStyle(s"strokeColor=#$color", targets.asInstanceOf[Array[Object]])
//    }
//
//    /** --------
//     * coloring
//     * -------- */
//    val postVertices = ArrayBuffer[V]()
//    val postEdges = ArrayBuffer[E]()
//
//    var currents: Seq[E] = dag.outputs.flatMap(_.incomingEdges)
//    while (currents.nonEmpty) {
//      val drivingVertices = currents.map(_.source).filter(v => v.isIo)
//      val drivingEdges = drivingVertices.flatMap(_.incomingEdges)
//      postEdges ++= drivingEdges
//      postVertices ++= drivingVertices
//      currents = drivingEdges
//    }
//
//    colorVertices(postVertices, "CCCC00")
//    colorEdges(postEdges, "CCCC00")
//
//    /** --------
//     * constructing layout
//     * -------- */
//
//    var vertexNow = dag.outputs
//    var level = 0
//
//    while (vertexNow.nonEmpty) {
//      val newVertex = new ArrayBuffer[DagVertex]
//
//      for (i <- vertexNow.indices) {
//        val g = vertexMap.get(vertexNow(i)).getGeometry
//        g.setRect(100 * i, 100 * level, g.getWidth, g.getHeight)
//
//        val edges = dag.incomingEdgesOf(vertexNow(i))
//        val edgesIter = edges.iterator()
//        while (edgesIter.hasNext) {
//          val verTex = dag.getEdgeSource(edgesIter.next)
//          if (!newVertex.contains(verTex)) {
//            newVertex += verTex
//          }
//        }
//      }
//
//      vertexNow = newVertex
//      level += 1
//    }
//
//    // using the mxGraph built-in CompactTreeLayout as our beginning
//    //    val layout = new mxCompactTreeLayout(graphAdapter, true, true)
//    //    // settings
//    //    layout.setMoveTree(true)
//    //    layout.setEdgeRouting(false)
//    //    layout.setResizeParent(false)
//    //    layout.setLevelDistance(5)
//    //
//    //    layout.execute(graphAdapter.getDefaultParent) // starts from a tree layout
//
//    // customization, according to the pipeline information
//    // TODO: better layout algo
//
//    //    def doLayoutByRetimingInfo(): Unit = {
//    //      val timeMax = dag.retimingInfo.values.max
//    //      val initialView = layout.getGraph.getView
//    //
//    //      def adjustY(v: V, y: Double) = {
//    //        val cell = vertexMap.get(v)
//    //        layout.setVertexLocation(cell, initialView.getState(cell).getX, y)
//    //      }
//    //
//    //      val pipelineGap = 50
//    //      dag.inputs.zipWithIndex.foreach { case (v, _) => adjustY(v, 0) }
//    //      dag.outputs.zipWithIndex.foreach { case (v, _) => adjustY(v, (timeMax + 2) * pipelineGap) }
//    //
//    //      dag.retimingInfo
//    //        .filterNot(_._1.isIo)
//    //        .groupBy(_._2).foreach { case (_, vToInt) => vToInt.foreach { case (v, i) => adjustY(v, (i + 1) * pipelineGap) }
//    //      }
//    //    }
//
//    //    doLayoutByRetimingInfo
//
//    /** --------
//     * png generation
//     * -------- */
//    val image = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
//    val fileName = if (pngName == null) dag.name else pngName
//    val imgFile = new File(dagFigDir, s"$fileName.png")
//    logger.info(s"view png generated for ${dag.name} in \n${imgFile.getAbsoluteFile}")
//    ImageIO.write(image, "PNG", imgFile)
//  }
//}
