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
    pythonExporter(dataFile).add(data: _*).savez()
    val dataIn = pythonImporter(dataFile).importBigDecimal
    data.zip(dataIn).foreach(d => assert(d._1 == d._2))
  }

  "array IO BigDecimal with name" should "work" in {
    val name = "data1"
    val data = Seq.fill(4096)(BigDecimal.valueOf(Random.nextDouble()))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    pythonExporter(dataFile).add(Map(name -> data)).savez()
    val dataIn = pythonImporter(dataFile).importNamedBigDecimal
    println(dataIn.keys)
    assert(data == dataIn(name))
  }

  "array IO BigDecimal with name" should "work with more data" in {
    val name = Seq("data1", "data2", "data3")
    val data = Seq.fill(name.size)(Seq.fill(4096)(BigDecimal.valueOf(Random.nextDouble())))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    pythonExporter(dataFile).add(name.zip(data).toMap).savez()
    val dataIn = pythonImporter(dataFile).importNamedBigDecimal
    println(dataIn.keys)
    name.indices.foreach(i => assert(data(i) == dataIn(name(i))))
  }

  "array IO BigDecimal 2d" should "work" in {
    val rows = 10
    val cols = 10
    val name = "data"
    val data = Seq.fill(rows)(Seq.fill(cols)(BigDecimal.valueOf(Random.nextDouble())))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    pythonExporter(dataFile).add2d(Map(name -> data)).savez()
    val dataIn = pythonImporter(dataFile).importNamedBigDecimal2d
    println("dataIn size1:" + dataIn.values.head.size + " dataIn size2: " + dataIn.values.head.head.size)
    println("data size1:" + data.size + " data size2: " + data.head.size)
    data.zip(dataIn(name)).foreach(d => assert(d._1 == d._2))
  }

  "array IO BigDecimal 2d" should "work with more data" in {
    val rows = 10
    val cols = 10
    val name = Seq("data1", "data2", "data3")
    val data = Seq.fill(3)(Seq.fill(rows)(Seq.fill(cols)(BigDecimal.valueOf(Random.nextDouble()))))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    pythonExporter(dataFile).add2d(name.zip(data).toMap).savez()
    val dataIn = pythonImporter(dataFile).importNamedBigDecimal2d
    println("dataIn size1:" + dataIn.values.head.size + " dataIn size2: " + dataIn.values.head.head.size)
    println("data size1:" + data.head.size + " data size2: " + data.head.head.size)
    name.indices.foreach(i => {
      data(i).zip(dataIn(name(i))).foreach(d => assert(d._1 == d._2))
    })
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
    pythonExporter(dataFile).addDouble(data: _*).savez()
    val dataIn = pythonImporter(dataFile).importDouble
    data.zip(dataIn).foreach(d => assert(d._1 == d._2))
  }

  "array IO Double with name" should "work" in {
    val name = "data"
    val data = Seq.fill(4096)(Random.nextDouble())
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    pythonExporter(dataFile).addDouble(Map(name -> data)).savez()
    val dataIn = pythonImporter(dataFile).importNamedDouble
    println(dataIn.keys)
    assert(data == dataIn(name))
  }

  "array IO Double with name" should "work with more data" in {
    val name = Seq("data1", "data2", "data3")
    val data = Seq.fill(name.size)(Seq.fill(10)(Random.nextDouble()))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    pythonExporter(dataFile).addDouble(name.zip(data).toMap).savez()
    val dataIn = pythonImporter(dataFile).importNamedDouble
    println(dataIn.keys)
    name.indices.foreach(i => assert(data(i) == dataIn(name(i))))
  }

  "array IO Double 2d" should "work" in {
    val rows = 10
    val cols = 10
    val name = "data"
    val data = Seq.fill(rows)(Seq.fill(cols)(Random.nextDouble()))
    val dataFile = new File(pythonDataPath, "pythonIoTestData.npz")
    pythonExporter(dataFile).addDouble2d(Map(name -> data)).savez()
    val dataIn = pythonImporter(dataFile).importNamedDouble2d
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
    pythonExporter(dataFile).addDouble2d(name.zip(data).toMap).savez()
    val dataIn = pythonImporter(dataFile).importNamedDouble2d
    println("dataIn size1:" + dataIn.values.head.size + " dataIn size2: " + dataIn.values.head.head.size)
    println("data size1:" + data.head.size + " data size2: " + data.head.head.size)
    name.indices.foreach(i => {
      data(i).zip(dataIn(name(i))).foreach(d => assert(d._1 == d._2))
    })
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
        val dataIn = importSignal(dataFile)
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

}
