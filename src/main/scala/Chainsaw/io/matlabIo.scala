package Chainsaw.io

// to resolve this, File -> project structure -> modules -> Chainsaw -> dependencies, add
// <MatlabPath>/extern/engines/java/jar/engine.jar
// <MatlabPath>/sys/os/glnxa64
// <MatlabPath>/bin/glnxa64

import com.mathworks.engine.MatlabEngine
import com.mathworks.matlab.types
import spinal.core._

import java.nio.file.Paths
import scala.io.Source

object matlabIo {

  /** Define your Matlab working space's path.
    */
  var matlabWorkingSpace = java.nio.file.Paths.get("./matlabWorkspace")

  val AsyncEng = if (MatlabEngine.findMatlab().nonEmpty) {
    MatlabEngine.connectMatlabAsync(MatlabEngine.findMatlab()(0))
  } else MatlabEngine.startMatlabAsync()

  val eng: MatlabEngine = AsyncEng.get()

  type MComplex      = types.Complex
  type MStruct       = types.Struct
  type MHandleObject = types.HandleObject
  type MCellStr      = types.CellStr

  /** Write content to a file in matlabWorkingSpace
    * @param fileName
    *   String
    * @param content
    *   String
    */
  def writeFile(fileName: String, content: String) = {
    val filepath = Paths.get(matlabWorkingSpace.toString, fileName)
    val writer   = new java.io.FileWriter(filepath.toFile)
    writer.write(content)
    writer.flush()
    writer.close()
  }

  /** Write content to a file incrementally
    * @param fileName
    *   String
    * @param content
    *   String
    */
  def writeFileIncrementally(fileName: String, content: String) = {
    val filepath   = Paths.get(matlabWorkingSpace.toString, fileName)
    val oldContent = Source.fromFile(filepath.toString).getLines().mkString("\n")
    val writer     = new java.io.FileWriter(filepath.toFile)
    writer.write(oldContent + content)
    writer.flush()
    writer.close()
  }

  /** Utils used for Array.
    */
  implicit class ArrayUtil(array: Array[_]) {
    def info: String = { // valid for "pure" array only
      var typeString = ""

      def recursiveBuild(current: Any, ret: Seq[String] = Seq[String]()): Seq[String] = {
        current match {
          case array: Array[_] => recursiveBuild(array(0), ret :+ array.size.toString)
          case value => {
            typeString = value.getClass.toString
            ret
          }
        }
      }

      s"[${recursiveBuild(array).mkString("x")} ${typeString.split('.').last}]"
    }

    def formatted: String = {
      var deepest = 0

      def recursiveBuild(element: Any, depth: Int = 0): String = {
        deepest = deepest max depth

        def sep = if ((deepest - depth) == 1) " " else "\n" * (deepest - depth - 1)

        element match {
          case array: Array[_] => array.map(recursiveBuild(_, depth + 1)).mkString(sep)
          case _               => element.toString
        }
      }

      recursiveBuild(array)
    }
  }

  /** Utils used for Matlab's Struct type.
    */
  implicit class StructUtil(struct: MStruct) {
    def formatted = {
      struct
        .keySet()
        .toArray
        .zip(struct.values().toArray)
        .map { case (key, value) =>
          val valueString = value match {
            case array: Array[_] => array.info
            case value           => value.toString
          }
          s"$key: $valueString"
        }
        .mkString("\n")
    }
  }

  /** Create one-dimensional Matlab array from Seq.
    */
  implicit class MatlabArray[T](array: Seq[T]) {
    def asMatlab = "[" + array.mkString(", ") + "]"
  }

  /** Create two-dimensional Matlab array from Seq.
    */
  implicit class MatlabArray2[T](array: Seq[Seq[T]]) {
    def asMatlab = "[" + array.map(_.mkString(", ")).mkString("; ") + "]"
  }

  /** Create complex type from Matlab complex Type "MComplex".
    */
  implicit class MComplexUtil(complex: MComplex) {
    def toBComplex = breeze.math.Complex(complex.real, complex.imag)
  }

  /** Utils used for MatlabEngine.
    */
  implicit class EngUtil(eng: MatlabEngine) {
    def setWorkingDir(workingDir: String): Unit = eng.eval(s"cd $workingDir")

    /** Get fileName as type [T].
      */
    def load[T](fileName: String): T = {
      eng.eval(s"load $fileName")
      eng.getVariable(fileName).asInstanceOf[T]
    }

    /** Get variable from a file through Matlab working space.
      */
    def load[T](fileName: String, varName: String): T = {
      eng.eval(s"load $fileName")
      eng.getVariable(varName).asInstanceOf[T]
    }

    /** Get variable from Matlab as SFix.
      */
    def getSFixType(variableName: String) = {
      eng.eval(
        s"temp0 = ${variableName}.numerictype.Signedness;\n" +
          s"temp1 = ${variableName}.numerictype.WordLength;\n" +
          s"temp2 = ${variableName}.numerictype.FractionLength;\n"
      )
      val signedString = eng.getVariable("temp0").asInstanceOf[String]
      if (signedString != "Signed") throw new IllegalArgumentException(s"$variableName is not signed")
      val wordLength     = eng.getVariable("temp1").asInstanceOf[Double].toInt
      val fractionLength = eng.getVariable("temp2").asInstanceOf[Double].toInt
      HardType(SFix((wordLength - 1 - fractionLength) exp, -fractionLength exp))
    }

    /** Get variable from Matlab as fixed raws.
      */
    def getFixRaws(variableName: String) = {
      eng.eval(s"temp = int($variableName);")
      eng.getVariable("temp").asInstanceOf[Array[Int]]
    }
  }
}
