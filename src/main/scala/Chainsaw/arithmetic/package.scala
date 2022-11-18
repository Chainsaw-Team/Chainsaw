package Chainsaw

import Chainsaw.dag.{DagPort, Resize, Split, SplitN}

import java.io.File
import Chainsaw.dag._

package object arithmetic {

  val compressorSolutionDir = {
    val dir = new File("src/main/resources/compressorSolutions")
    dir.mkdirs()
    dir
  }

  val defaultCompressorPerfGraphPath = new File("src/main/resources/compressorPerfs")

  val ternaryWidthMax = 96
  val binaryWidthMax = 96

  def getBinaryWidths(fullWidth: Int) = {
    val n = fullWidth.divideAndCeil(binaryWidthMax)
    (fullWidth - (n - 1) * binaryWidthMax) +: Seq.fill(n - 1)(binaryWidthMax)
  }

  implicit class karatsubaPortUtil(port: DagPort)(implicit dag: Dag) {

    def splitAt(lowWidth: Int) = {
      val s = Split(port.width, lowWidth).asVertex
      s := port
      (s.out(0), s.out(1))
    }

    def takeLow(width: Int) = port.splitAt(width)._2

    def takeHigh(width: Int) = port.splitAt(port.width - width)._1

    def splitN(n: Int) = {
      val s = SplitN(port.width, n).asVertex
      s := port
      s.outPorts
    }

    def +^(that: DagPort) = {
      val add = CpaS2S(BinaryAdder, that.width max port.width, withCarry = true).asVertex
      add := (port, that)
      add.out(0)
    }

    def resize(widthOut: Int) = {
      val re = Resize(port.width, widthOut).asVertex
      re := port
      re.out(0)
    }

    def <<(shiftLeft: Int) = {
      val shift = ShiftLeft(shiftLeft, port.width).asVertex
      shift := port
      shift.out(0)
    }
  }
}
