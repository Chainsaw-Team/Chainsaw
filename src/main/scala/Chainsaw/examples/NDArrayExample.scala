package Chainsaw.examples

import ai.djl.ndarray._
import ai.djl.ndarray.types._

import java.io.OutputStream
import java.nio.file.{Files, Paths}

object NDArrayExample extends App {

  val manager = NDManager.newBaseManager()
  val ndArray = manager.ones(new Shape(2, 3))

  val encoded = new NDList(ndArray)
  encoded.encode()
  val os = Files.newOutputStream(Paths.get("ndarray.npz"))
  encoded.encode(os, true)
}
