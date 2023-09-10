package Chainsaw.io

import Chainsaw.io.pythonIo._
import breeze.plot._

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class pythonIoTest extends org.scalatest.flatspec.AnyFlatSpec {

  "array IO BigDecimal" should "work" in {
    val data = Seq.fill(4096)(BigDecimal.valueOf(Random.nextDouble()))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignal(dataFile, data)
    val dataIn = importSignal(dataFile)
    assert(data == dataIn)
  }

  "array IO BigDecimal" should "work with more data" in {
    val data = Seq.fill(3)(Seq.fill(4096)(BigDecimal.valueOf(Random.nextDouble())))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignals(dataFile, data:_*)
    val dataIn = importSignals(dataFile)
    data.zip(dataIn).foreach(d => assert(d._1 == d._2))
  }

  "array IO BigDecimal with name" should "work" in {
    val name = "data1"
    val data = Seq.fill(4096)(BigDecimal.valueOf(Random.nextDouble()))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignals(dataFile, Map(name -> data))
    val dataIn = importSignalsNamedDouble(dataFile)
    println(dataIn.keys)
    assert(data == dataIn(name))
  }

  "array IO BigDecimal with name" should "work with more data" in {
    val name = Seq("data1", "data2", "data3")
    val data = Seq.fill(name.size)(Seq.fill(4096)(BigDecimal.valueOf(Random.nextDouble())))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignals(dataFile, name.zip(data).toMap)
    val dataIn = importSignalsNamedDouble(dataFile)
    println(dataIn.keys)
    name.indices.foreach(i => assert(data(i) == dataIn(name(i))))
  }

  "array IO Double" should "work" in {
    val data = Seq.fill(4096)(Random.nextDouble())
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignalDouble(dataFile, data)
    val dataIn = importSignalDouble(dataFile)
    assert(data == dataIn)
  }

  "array IO Double" should "work with more data" in {
    val data = Seq.fill(3)(Seq.fill(4096)(Random.nextDouble()))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignalsDouble(dataFile, data: _*)
    val dataIn = importSignalsDouble(dataFile)
    data.zip(dataIn).foreach(d => assert(d._1 == d._2))
  }

  "array IO Double with name" should "work" in {
    val name = "data"
    val data = Seq.fill(4096)(Random.nextDouble())
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignalsDouble(dataFile, Map(name -> data))
    val dataIn = importSignalsNamedDouble(dataFile)
    println(dataIn.keys)
    assert(data == dataIn(name))
  }

  "array IO Double with name" should "work with more data" in {
    val names = Seq("data1", "data2", "data3")
    val data = Seq.fill(names.size)(Seq.fill(10)(Random.nextDouble()))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignalsDouble(dataFile, names.zip(data).toMap)
    val dataIn = importSignalsNamedDouble(dataFile)
    println(dataIn.keys)
    names.indices.foreach(i => assert(data(i) == dataIn(names(i))))
  }

  "array IO Double 2d" should "work" in {
    val rows = 10
    val cols = 10
    val name = "data"
    val data = Seq.fill(rows)(Seq.fill(cols)(Random.nextDouble()))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignalsDouble2d(dataFile, Map(name -> data))
    val dataIn = importSignalsDouble2d(dataFile, (rows, cols))
    println("dataIn size1:" + dataIn.values.head.size + " dataIn size2: " + dataIn.values.head.head.size)
    println("data size1:" + data.size + " data size2: " + data.head.size)
    data.zip(dataIn(name)).foreach(d => assert(d._1 == d._2))
  }

  "array IO Double 2d" should "work with more data" in {
    val rows = 10
    val cols = 10
    val name = Seq("data1", "data2", "data3")
    val data = Seq.fill(3)(Seq.fill(rows)(Seq.fill(cols)(Random.nextDouble())))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignalsDouble2d(dataFile, name.zip(data).toMap)
    val dataIn = importSignalsDouble2d(dataFile, (rows, cols))
    println("dataIn size1:" + dataIn.values.head.size + " dataIn size2: " + dataIn.values.head.head.size)
    println("data size1:" + data.head.size + " data size2: " + data.head.head.size)
    name.indices.foreach(i => {
      data(i).zip(dataIn(name(i))).foreach(d => assert(d._1 == d._2))
    })
  }

  "convert 2d 1d " should "work" in {
    val rows = 10
    val cols = 10
    val name = Seq("data1", "data2")
    val matrixData1 = Seq.fill(rows)(Seq.fill(cols)(Random.nextDouble()))
    val matrixData2 = Seq.fill(rows)(Seq.fill(cols)(Random.nextDouble()))
    val data = Seq.fill(2)(Seq.fill(100)(Random.nextDouble()))
    val namedData = name.zip(data).toMap ++ flatten2dSignalsDouble(Map("matrixData1" -> matrixData1, "matrixData2" -> matrixData2))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    exportSignalsDouble(dataFile, namedData)
    val dataIn = importSignalsNamedDouble(dataFile)
    val matrixDataIn = fold2dSignalsDouble(dataIn)

    println("matData1In size1:" + matrixDataIn("matrixData1").size + " matData1In size2: " + matrixDataIn("matrixData1").head.size)
    println("matData1 size1:" + matrixData1.size + " matData1 size2: " + matrixData1.head.size)
    println("matData2In size1:" + matrixDataIn("matrixData2").size + " matData2In size2: " + matrixDataIn("matrixData2").head.size)
    println("matData2 size1:" + matrixData2.size + " matData2 size2: " + matrixData2.head.size)
    matrixData1.zip(matrixDataIn("matrixData1")).foreach(d => assert(d._1 == d._2))
    matrixData2.zip(matrixDataIn("matrixData2")).foreach(d => assert(d._1 == d._2))
    name.indices.foreach(i => assert(data(i) == dataIn(name(i))))
  }


  val dataLenLevel =  (10 until 20 by 1).map(_.toDouble) ++
    (BigDecimal(20) until BigDecimal(24) by 0.25).map(_.toDouble)
  val dataLen = dataLenLevel.map(math.pow(2, _))
  println(dataLen)

  val saveResultLen: ArrayBuffer[Double] = ArrayBuffer()
  val saveResultTime: ArrayBuffer[Double] = ArrayBuffer()
  val loadResultLen: ArrayBuffer[Double] = ArrayBuffer()
  val loadResultTime: ArrayBuffer[Double] = ArrayBuffer()

  "array IO BigDecimal" should "work with large data" in {
    dataLen.foreach(len => {
      (0 to 5).foreach { _ =>
        val startTime = System.currentTimeMillis()

        val data = Seq.fill(len.floor.toInt)(BigDecimal.valueOf(Random.nextDouble()))
        val dataFile = new File(pythonDataPath, s"pythonIoTestData_${len.floor.toInt}.npz")
        exportSignal(dataFile, data)
        val dataIn = importSignals(dataFile).head
        assert(data == dataIn)

        val elapsedTime = System.currentTimeMillis() - startTime
        println(s"array IO test with len $len finished in $elapsedTime")

        saveResultLen.append(len)
        saveResultTime.append(elapsedTime)
      }
    })

    // TODO: draw a regression line
    val f = Figure()
    val p = f.subplot(0)
    p += plot(saveResultLen, saveResultTime, '.')
    p.xlabel = "length"
    p.ylabel = "time in millis"
    p.title = "pythonIo array time"
    f.saveas("pythonIoTime.png")

    println(s"len $saveResultLen")
    println(s"len $saveResultTime")
  }

  "array IO Double" should "work with large data" in {
    dataLen.foreach(len => {
      (0 to 5).foreach { _ =>
        val startTime = System.currentTimeMillis()

        val data = Seq.fill(len.floor.toInt)(Random.nextDouble())
        val dataFile = new File(pythonDataPath, s"pythonIoTestData_${len.floor.toInt}.npz")
        exportSignalDouble(dataFile, data)
        val dataIn = importSignalDouble(dataFile)
        assert(data == dataIn)

        val elapsedTime = System.currentTimeMillis() - startTime
        println(s"array IO test with len $len finished in $elapsedTime")

        saveResultLen.append(len)
        saveResultTime.append(elapsedTime)
      }
    })

    // TODO: draw a regression line
    val f = Figure()
    val p = f.subplot(0)
    p += plot(saveResultLen, saveResultTime, '.')
    p.xlabel = "length"
    p.ylabel = "time in millis"
    p.title = "pythonIo Double array time"
    f.saveas("pythonIoDoubleTime.png")

    println(s"len $saveResultLen")
    println(s"len $saveResultTime")
  }

  "array IO" should "check whether data size exceed ram size" in {
    // TODO: unable to test
    val test = importSignalsDouble(new File(pythonDataPath, "pythonIoTestData_1048576.npz"))
  }

}
