package Chainsaw.examples

import java.io._

object FileExample extends App {

  val dir = new File("./src/main/resources/temp/temp")
  val file = new File(dir, "temp.json")

  println("\nattributes")

  println(s"dir.isDirectory = ${dir.isDirectory}")
  println(s"dir.isFile = ${dir.isFile}")
  println(s"dir.isAbsolute = ${dir.isAbsolute}")
  println(s"dir.exists() = ${dir.exists()}")

  println("\nget")

  println(s"dir.getPath = ${dir.getPath}")
  println(s"dir.getParent = ${dir.getParent}")
  println(s"dir.getParentFile = ${dir.getParentFile}")
  println(s"dir.getAbsolutePath = ${dir.getAbsolutePath}")
  println(s"dir.getCanonicalPath = ${dir.getCanonicalPath}")

  println("\nactions")

  println(s"dir.mkdir() = ${dir.mkdir()}") // failed as its parent doesn't exist
  println(s"dir.mkdirs() = ${dir.mkdirs()}") // recursive version of mkdir, succeed
  println(s"dir.createNewFile() = ${dir.createNewFile()}") // failed as it is not a file
  println(s"file.createNewFile() = ${file.createNewFile()}") // succeed
  println(s"file.delete() = ${file.delete()}")
  println(s"dir.delete() = ${dir.delete()}")

  println("\nothers")


}
