package Chainsaw.edaFlow

import spinal.core.{GlobalData, SpinalError}
import spinal.lib.bus.regif._

import java.io.PrintWriter
import scala.collection.mutable

case class PythonHeaderGenerator(
    fileName: String,
    prefix: String,
    regType: String = "u32"
) extends BusIfVisitor {
  val words = "\\w*".r
  prefix match {
    case words(_*) => null
    case _         => SpinalError(s"${prefix} should be Valid naming : '[A-Za-z0-9_]+'")
  }

  case class Reg(name: String, addr: Long)

  case class Field(name: String, width: Long, accessType: AccessType)

  case class Type(name: String, var fields: List[FieldDescr])

  val regs: mutable.ListBuffer[RegDescr] = mutable.ListBuffer[RegDescr]()
  val types: mutable.ListBuffer[Type]    = mutable.ListBuffer[Type]()
  var regLength: Int                     = 0

  def begin(busDataWidth: Int): Unit = {}

  def visit(descr: BaseDescriptor): Unit = {
    descr match {
      case descr: RegDescr => regDescrVisit(descr)
      case _               => ???
    }
  }

  private def regDescrVisit(descr: RegDescr): Unit = {
    def nameLen = descr.getName.length()

    if (nameLen > regLength)
      regLength = nameLen

    regs += descr
    types += Type(descr.getName, descr.getFieldDescrs)
  }

  def end(): Unit = {
    val pc         = GlobalData.get.phaseContext
    val targetPath = s"${pc.config.targetDirectory}/${fileName}.py"
    val pw         = new PrintWriter(targetPath)

    regs.zip(types).foreach { case (reg, t) =>
      val regAddress   = s"0x%0${4}x".format(reg.getAddr)
      val fieldWidths  = t.fields.map(_.getWidth)
      val fieldOffsets = fieldWidths.scan(0)(_ + _).init
      val fieldNames   = t.fields.filterNot(_.getAccessType() == AccessType.NA).map(_.getName)

      // a class supporting read/write for each reg
      // TODO: read methods for different fields of a register
      pw.write(s"""
           |class ${t.name.toLowerCase()}:
           |    def __init__(self, ${fieldNames.mkString(", ")}):
           |        self.address = $regAddress
           |        self.field_widths = [${fieldWidths.mkString(", ")}]
           |        ${fieldNames.map(name => s"self.$name = $name").mkString("\n        ")}
           |
           |    def value(self):
           |        return ${fieldNames.zip(fieldOffsets).reverse.map { case (name, shift) => s"self.$name << $shift" }.mkString(" | ")}
           |""".stripMargin)
    }

    val regByteCount: Int = types.head.fields.map(_.getWidth()).sum / 8

    pw.write(s"""
         |
         |def update_reg(reg):
         |    file_name = r"\\\\.\\xillybus_mem_32"  # replace with your file_name
         |    byte_value = reg.value().to_bytes(${regByteCount}, byteorder='little')
         |    with open(file_name, "wb") as ctrl:
         |        ctrl.seek(reg.address)
         |        ctrl.write(byte_value)
         |    # time.sleep(0.1)
         |    # print(f"{reg.__class__.__name__} value = {reg.value()}")
         |    # with open(file_name, "rb") as ctrl:
         |    #     ctrl.seek(reg.address)
         |    #     ret = ctrl.read(${regByteCount})
         |    #     assert byte_value == ret, f"failed to update reg {reg.__class__.__name__}, target: {byte_value} != actual: {ret}"
         |        
         |""".stripMargin)
    pw.close()
  }
}
