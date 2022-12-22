package Chainsaw.arithmetic

import scala.collection.mutable.ArrayBuffer

package object bitheap {

  def getCompressor(compressorName: String, width: Int) =
    compressorName match {
      case "Compressor2117" => Compressor2117
      case "Compressor6to3" => Compressor6to3
      case "Compressor3to1" => Compressor3to1(width)


      // TODO: fill up the rest
    }


}
