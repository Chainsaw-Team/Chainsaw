package Chainsaw.arithmetic

import Chainsaw.StringUtil

import scala.collection.mutable.ArrayBuffer

package object bitheap {

  val candidateCompressors = RowAdders() ++ Gpcs()

  def getCompressor(compressorName: String, width: Int = -1): CompressorGenerator = {
    compressorName match {
      case "Compressor1to1" => Compressor1to1(width)
      case "Compressor2117" => Compressor2117
      case "Compressor3to2" => Compressor3to2
      case "Compressor6to3" => Compressor6to3
      case "Compressor3to1" => Compressor3to1(width)
      case "Compressor4to2" => Compressor4to2(width)

      // TODO: fill up the rest
    }
  }
}
