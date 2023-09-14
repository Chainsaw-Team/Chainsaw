package Chainsaw.io

import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source
import scala.util.Random

class matlabIoTest extends AnyFlatSpec {
  matlabIo.matlabWorkingSpace = java.nio.file.Paths.get("/home/lq/MatlabFiles/")
  val eng = matlabIo.eng
  matlabIo.EngUtil(eng).setWorkingDir("/home/lq/MatlabFiles/")
  val testArray = Array.fill(2,10)(Random.nextDouble())
  val testArrayAsSrting = testArray.map(_.mkString(" ")).mkString("\n")

  "WriteFile" should "do correctly" in{
    val writeFileName = "ioTestFile1.txt"
    matlabIo.writeFile(writeFileName, testArrayAsSrting)
    val readFile = Source.fromFile("/home/lq/MatlabFiles/"+writeFileName)
    val lines = readFile.getLines()
    assert(lines.mkString("\n") == testArray.map(_.mkString(" ")).mkString("\n"))
  }

  "WriteFileIncrementally" should "do correctly" in{
    val writeFileName = "ioTestFile2.txt"
    matlabIo.writeFile(writeFileName, testArrayAsSrting)
    matlabIo.writeFileIncrementally(writeFileName, testArrayAsSrting)
    val readFile = Source.fromFile("/home/lq/MatlabFiles/"+writeFileName)
    val lines = readFile.getLines()
    assert(lines.mkString("\n") == testArray.map(_.mkString(" ")).mkString("\n")+testArray.map(_.mkString(" ")).mkString("\n"))
  }

  "Array IO" should "work" in{
    val testData = Array.fill(10)(Random.nextDouble())
    eng.putVariable("testData", testData)
    eng.eval("a=cos(testData)")
    print(eng.getVariable[Array[Double]]("a").mkString(" "))
    val returnData = eng.getVariable[Array[Double]]("testData")
    for(i <- 0 until testData.length) {
      assert(testData(i) == returnData(i))
    }
    val compareData = testData.zip(returnData)
    assert(compareData.forall{case(a,b) => a==b})
  }

  "feval" should "work" in {
    val fevalResult = eng.feval[Double]("sin", Array(0.5))
    val scalaResult = Math.sin(0.5)
    assert(fevalResult == scalaResult)
  }

  "eval" should "work" in {
    eng.putVariable[Double]("input", 10)
    eng.eval("sin(input)")
    val evalResult = eng.getVariable[Double]("ans")
    val scalaResult = Math.sin(10)
    assert(evalResult == scalaResult)
  }

  "MatlabArray and MatlabArray2" should "work" in {
    val testSeq = Seq.fill(5)(Random.nextDouble())
    val testSeq2 = Seq.fill(5,5)(Random.nextDouble())
    val matlabSeq = matlabIo.MatlabArray[Double](testSeq).asMatlab
    val matlabSeq2 = matlabIo.MatlabArray2[Double](testSeq2).asMatlab
    eng.eval(s"a=sin($matlabSeq)")
    eng.eval(s"b=sin($matlabSeq2)")
    val evalResult = eng.getVariable[Array[Double]]("a")
    val scalaResult = testSeq.map(Math.sin(_))
    val evalResult2 = eng.getVariable[Array[Array[Double]]]("b")
    val scalaResult2 = testSeq2.map(_.map(Math.sin(_)))
    val compareData = evalResult.zip(scalaResult)
    assert(compareData.forall{case(a,b) => a==b})
    for(i <- 0 until 4) {
      for(j <- 0 until 4){
        assert(evalResult2(i)(j) == scalaResult2(i)(j))
      }
    }
  }
}

