package Chainsaw.arithmetic

import Chainsaw.StringUtil

import scala.collection.mutable.ArrayBuffer

package object bitheap {

  val candidateCompressors = RowAdders() ++ Gpcs()

  def getCompressor(compressorName: String, width: Int = -1, complementHeap: Seq[Seq[Boolean]] = null): CompressorGenerator = {
    compressorName match {
      case "Compressor1to1" => Compressor1to1(width, complementHeap)
      case "Compressor3to2" => Compressor3to2(complementHeap)
      case "Compressor6to3" => Compressor6to3(complementHeap)
      case "Compressor3to1" => Compressor3to1(width, 0, complementHeap)
      case "Compressor4to2" => Compressor4to2(width, complementHeap)
    }
  }
}