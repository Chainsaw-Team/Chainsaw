package Chainsaw.examples

import ai.djl.ndarray._
import ai.djl.ndarray.types._

import java.io.OutputStream
import java.nio.file.{Files, Paths}

object NDArrayExample extends App {

  val manager = NDManager.newBaseManager()
  val ndArray = manager.ones(new Shape(1000, 1000))

  val encoded = new NDList(ndArray)
  encoded.encode()
  val os = Files.newOutputStream(Paths.get("ndarray.npz"))
  encoded.encode(os, true)

  val is        = Files.newInputStream(Paths.get("ndarray.npz"))
  val decoded   = NDList.decode(manager, is)
  val recovered = decoded.get(0)

  println(s"number of arrays: ${decoded.size()}")
  println(s"recovered ${recovered}")
  println(s"recovered array ${recovered.toFloatArray.mkString("\n")}")
  println(s"recovered ${recovered.getFloat(0, 0)}")
}
