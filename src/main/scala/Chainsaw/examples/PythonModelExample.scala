package Chainsaw.examples

import ai.djl.ndarray._

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Files, Paths}

object PythonModelExample extends App {

  val yours  = Seq(1, 2, 3)
  val golden = Seq(4, 5, 6)

  val manager = NDManager.newBaseManager()

  // export data
  val pair =
    new NDList(manager.create(yours.toArray), manager.create(golden.toArray))
  val os = Files.newOutputStream(Paths.get("pair.npz"))
  pair.encode(os, true)

  // run python metric
  val process: Process =
    Runtime.getRuntime.exec(
      "/home/ltr/anaconda3/bin/python /home/ltr/IdeaProjects/Chainsaw/goldenModel/corrMetric.py"
    ); // 执行py文件
  val in = new BufferedReader(new InputStreamReader(process.getInputStream))

  var line = in.readLine()
  while (line != null) {
    println(line)
    line = in.readLine()
  }

  in.close()

}
