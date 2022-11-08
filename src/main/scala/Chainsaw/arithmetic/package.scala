package Chainsaw

import java.io.File

package object arithmetic {

  val compressorSolutionDir = {
    val dir =  new File("src/main/resources/compressorSolutions")
    dir.mkdirs()
    dir
  }

  val defaultCompressorPerfGraphPath = new File("src/main/resources/compressorPerfs")

}
