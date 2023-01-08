package Chainsaw.examples

//import StarX.intf._

import spinal.core.{B, _}
import spinal.lib.bus.regif.RegDescr

import java.io.{File, PrintWriter}
import java.lang.reflect.{Method, Type}
import scala.collection.mutable
import scala.collection.mutable._
import spinal.core.GlobalData.set
import spinal.core._
import spinal.lib._

object UML {
  val extendRelations: ListBuffer[String] = ListBuffer()
}

case class XdcNode(name: String, node: Data, children: ListBuffer[XdcNode] = ListBuffer()) {
  def getNodeByName(n: String): XdcNode = {
    var ret: XdcNode = null
    if (name == n) {
      ret = this
    } else {
      for (child <- children) {
        if (ret == null) {
          ret = child.getNodeByName(n)
        }
      }
    }
    ret
  }
}

object XDC {
  val constriants: ListBuffer[String] = ListBuffer()
  val clocks: ListBuffer[XdcNode] = ListBuffer()

  def findMaster(name: String): XdcNode = {
    var ret: XdcNode = null
    for (clock <- clocks) {
      if (ret == null) {
        ret = clock.getNodeByName(name)
      }
    }
    ret
  }
}

class IP() extends Component {
  this.setName(this.getClass().getSimpleName())
  noIoPrefix()
  globalData.commonClockConfig = ClockDomainConfig(
    clockEdge = RISING,
    resetKind = ASYNC,
    resetActiveLevel = LOW
  )

  def debugMode = false

  def markDebug(signal: Data): Unit = {
    if (debugMode) {
      if (!signal.isInput && !signal.isOutputOrInOut && signal.isReg) {
        signal.addAttribute("MARK_DEBUG", "TRUE")
      }
    }
  }

  def xdcGetNode(node: Data, handler: String = "Q"): String = {
    println(s"${node.toString()}")
    val node_path = node.getRtlPath()
    var get_node = s"get_nets ${node_path}"
    if (node.isDirectionLess) {
      if (node.isReg) {
        get_node = s"get_pins ${node_path}_reg.${handler}"
      }
    } else {
      if (globalData.toplevel == this) {
        get_node = s"get_ports ${node_path}"
      } else {
        get_node = s"get_pins ${node_path}"
      }
    }
    get_node
  }

  def xdcSetFalsePath(from: Data = null, to: Data = null, through: Data = null): Unit = {
    var s = "set_false_path"
    if (from != null) {
      s += s" -from [${xdcGetNode(from)}]"
    }
    if (through != null) {
      s += s" -through [${xdcGetNode(through)}]"
    }
    if (to != null) {
      s += s" -to [${xdcGetNode(to, "D")}]"
    }
    s += "\n"
    XDC.constriants += s
  }

  def xdcCreateGeneratedClock(name: String, divideBy: Int, node: Data): Unit = {
    xdcCreateGeneratedClock(name, (1, divideBy * 1 + 1, divideBy * 2 + 1), node)
  }

  def xdcCreateGeneratedClock(name: String, divideBy: Int, master: String, node: Data): Unit = {
    xdcCreateGeneratedClock(name, (1, divideBy * 1 + 1, divideBy * 2 + 1), master, node)
  }

  def xdcCreateGeneratedClock(name: String, divideBy: Int, src: Data, node: Data): Unit = {
    xdcCreateGeneratedClock(name, (1, divideBy * 1 + 1, divideBy * 2 + 1), src, node)
  }

  def xdcCreateGeneratedClock(name: String, edges: (Int, Int, Int), node: Data): Unit = {
    XDC.constriants += s"create_generated_clock -name ${name} -source [${xdcGetNode(node, "C")}] -edges {${edges._1} ${edges._2} ${edges._3}} [${xdcGetNode(node)}]\n"
  }

  def xdcCreateGeneratedClock(name: String, edges: (Int, Int, Int), master: String, node: Data): Unit = {
    val mst = XDC.findMaster(master)
    if (mst == null) {
      XDC.constriants += s"create_generated_clock -name ${name} -edges {${edges._1} ${edges._2} ${edges._3}} -add -master ${mst.name} [${xdcGetNode(node)}]\n"
    } else {
      XDC.constriants += s"create_generated_clock -name ${name} -source [${xdcGetNode(mst.node)}] -edges {${edges._1} ${edges._2} ${edges._3}} -add -master ${mst.name} [${xdcGetNode(node)}]\n"
    }
  }

  def xdcCreateGeneratedClock(name: String, edges: (Int, Int, Int), src: Data, node: Data): Unit = {
    XDC.constriants += s"create_generated_clock -name ${name} -source [${xdcGetNode(src)}] -edges {${edges._1} ${edges._2} ${edges._3}} [${xdcGetNode(node)}]\n"
  }

  def xdcCreateClock(name: String, freqMHz: Int, node: Data, dutyHighRatio: Double = 0.5): Unit = {
    val period = ((1 / (freqMHz * 1e6)) * 1e12).toInt * 1e-3
    val highTime = period * dutyHighRatio
    xdcCreateClock(name, period, node, (0.000, highTime))
  }

  def xdcCreateClock(name: String, period: Double, node: Data, waveform: (Double, Double)): Unit = {
    val clkName = s"${this.getName()}_${name}"
    XDC.constriants += s"create_clock -name ${clkName} -period ${period} -waveform {${waveform._1} ${waveform._2}} [${xdcGetNode(node)}]\n"
    XDC.clocks += XdcNode(clkName, node)
  }

  def generateXdcFile(xdc: PrintWriter = new PrintWriter(s"${this.getName()}.xdc"), xdcInit: Boolean = true): IP = {
    if (xdcInit) {
      xdc.write(s"#auto generated xdc start\n")
    }
    constraintPhase()
    if (this.children.length > 0) {
      for (child <- this.children) {
        if (child.isInstanceOf[IP]) {
          child.asInstanceOf[IP].generateXdcFile(xdc, false)
        }
      }
    }
    if (xdcInit) {
      for (constriant <- XDC.constriants) {
        xdc.write(constriant)
      }
      if (XDC.clocks.length > 0) {
        xdc.write(s"set_clock_groups -asynchronous")
        for (clock <- XDC.clocks) {
          xdc.write(s" -group [get_clocks -include_generated_clocks ${clock.name}]")
        }
        xdc.write(s"\n")
      }
      xdc.write(s"#auto generated xdc end\n")
      xdc.close()
    }
    this
  }

  def generateUmlFile(uml: PrintWriter = new PrintWriter(s"${this.getName()}.plantuml"), umlInit: Boolean = true): IP = {
    if (umlInit) {
      uml.write(s"@startuml\n")
      UML.extendRelations.clear()
    }
    uml.write(s"class ${this.getClass().getName()} {\n")
    val ios = this.getGroupedIO(false)
    if (ios.length > 0) {
      uml.write(s"  .. io ..\n")
      for (io <- ios) {
        if (io.getName() matches ("^io.*")) {
          uml.write(s"  + {field} ${io.getClass.toString.replaceAll("class ", "").replaceAll("\\w+\\.", "")} ${io.getName()}\n")
        }
      }
    }
    //    if (this.isInstanceOf[BusIP]) {
    //      uml.write(s"  .. regs ..\n")
    //      for (method <- this.getClass().getDeclaredMethods()) {
    //        val methodString = method.toString
    //        if (methodString.matches(".*RegInst .*")) {
    //          uml.write(s"  # {field} RegInst ${method.getName()}\n")
    //        }
    //      }
    //    }
    uml.write(s"  .. method ..\n")
    val methods = new HashMap[String, Method]
    for (method <- this.getClass().getDeclaredMethods()) {
      val methodString = method.toString
      if ((methodString.matches("^public spinal.*") || methodString.matches("^public StarX.*")) && !methodString.matches(".* static .*") && !methodString.matches(".*RegInst .*") && !methodString.matches(".*copy.*")) {
        if (!methods.contains(method.getName())) {
          methods(method.getName()) = method
        }
      }
    }
    for (method <- methods.values) {
      uml.write(s"${method.toString.replaceAll("public ", "  + {method} ").replaceAll("\\w+\\.", "")}\n")
    }
    if (this.children.length > 0) {
      uml.write(s"  .. inst ..\n")
      for (child <- this.children) {
        uml.write(s"  # {field} ${child.getName()}\n")
      }
    }
    uml.write(s"}\n")

    var c: Class[_ <: IP] = this.getClass()
    while (c != null) {
      if (c.getSuperclass() != null) {
        val extendRelation = s"${c.getSuperclass().getName()} <|-- ${c.getName()}"
        if (!UML.extendRelations.contains(extendRelation)) {
          uml.write(s"${extendRelation}\n")
          UML.extendRelations += extendRelation
        } else {
          //uml.write(s"checking >>> ${extendRelation}\n")
          //for(a <- UML.extendRelations) {
          //  uml.write(s"find >>> ${a}\n")
          //}
        }
        c = c.getSuperclass().asInstanceOf[Class[_ <: IP]]
      } else {
        c = null
      }
    }

    if (this.children.length > 0) {
      val children = new HashMap[String, IP]
      val childrenNum = new HashMap[String, Int]
      for (child <- this.children) {
        if (child.isInstanceOf[IP]) {
          children(child.getClass().getName()) = child.asInstanceOf[IP]
          if (childrenNum.contains(child.getClass().getName())) {
            childrenNum(child.getClass().getName()) += 1
          } else {
            childrenNum(child.getClass().getName()) = 1
          }
        }
      }
      for (child <- children.values) {
        child.generateUmlFile(uml, false)
        val q = '"'
        uml.write(s"${this.getClass().getName()} ${q}1${q} *-- ${q}${childrenNum(child.getClass().getName())}${q} ${child.getClass().getName()}\n")
      }
    }
    if (umlInit) {
      uml.write(s"@enduml\n")
      uml.close()
    }
    this
  }

  def copyAssignment(src: BaseType, tar: BaseType) = {
    if (!src.hasAssignement && tar.hasAssignement) {
      src := tar.getSingleDriver.get
    } else if (!tar.hasAssignement && src.hasAssignement) {
      tar := src.getSingleDriver.get
    }
  }

  final def buildIP(): Unit = {
    prePhase()
    buildPhase()
    postPhase()
    reportPhase()
  }

  def prePhase(): Unit = {
  }

  def buildPhase(): Unit = {
  }

  def postPhase(): Unit = {
  }

  def reportPhase(): Unit = {
  }

  def constraintPhase(): Unit = {

  }

  addPrePopTask(() => buildIP())
}

object IpTest extends App {

  case class IpExample() extends IP {
    val io = new Bundle {
      val a = in UInt (8 bits)
      val b = in UInt (8 bits)
      val c = out UInt (8 bits)
    }
    io.c := io.a + io.b

    generateUmlFile(new PrintWriter(new File("example")))
    generateXdcFile(new PrintWriter(new File("example.xdc")))
  }

  SpinalVerilog(IpExample())

}
