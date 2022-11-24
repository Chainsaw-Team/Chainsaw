package Chainsaw.examples

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

import Chainsaw._

object ReflectionExamples {

  def classAccessors[T: TypeTag]: List[MethodSymbol] = typeOf[T].members.collect {
    case m: MethodSymbol if m.isCaseAccessor => m // get fields, rather than methods
  }.toList


  case class Person(name: String)

  case class Purchase(name: String, orderNumber: Int, var shipped: Boolean) {
    def valuesAndName = {
      val mirror: universe.Mirror = runtimeMirror(getClass.getClassLoader)
      println(typeOf[this.type])
      val fieldSymbols = typeOf[this.type].members.filterNot(_.isMethod).map(_.asTerm).toSeq.reverse
      val fieldNames = fieldSymbols.map(_.name)
      val instanceMirror: universe.InstanceMirror = mirror.reflect(this)
      fieldSymbols.map(instanceMirror.reflectField).map(_.get).zip(fieldNames.map(_.toString))
    }
  }

  def getTypeTag[T: TypeTag](obj: T) = typeTag[T]

  def main(args: Array[String]): Unit = {
    // example1: Inspecting a Runtime Type
    val typeValue: universe.Type = getTypeTag(Seq(1, 2, 3)).tpe
    println(typeValue)
    // example2: Instantiating a Type at Runtime
    val mirror: universe.Mirror = runtimeMirror(getClass.getClassLoader)
    val classPerson: universe.ClassSymbol = typeOf[Person].typeSymbol.asClass
    val classMirror: universe.ClassMirror = mirror.reflectClass(classPerson)
    val constructor: universe.MethodSymbol = typeOf[Person].decl(termNames.CONSTRUCTOR).asMethod
    val constructorMirror: universe.MethodMirror = classMirror.reflectConstructor(constructor)
    val anotherPerson = constructorMirror("Mike")
    println(anotherPerson)
    // example3: Accessing and Invoking Members of Runtime Types
    val p = Purchase("Jeff Lebowski", 23819, shipped = false)
    val shippedTermSymbol: universe.TermSymbol = typeOf[Purchase].decl(TermName("shipped")).asTerm
    val instanceMirror: universe.InstanceMirror = mirror.reflect(p)
    val fieldMirror: universe.FieldMirror = instanceMirror.reflectField(shippedTermSymbol)
    fieldMirror.get
    // exmaple4
    println(p.valuesAndName)
  }

}
