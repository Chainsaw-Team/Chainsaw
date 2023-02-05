package Chainsaw.examples

import org.yaml.snakeyaml.Yaml
import scala.io.Source

// for details, see https://bitbucket.org/snakeyaml/snakeyaml/wiki/Documentation
object YamlExample extends App {

  val yaml       = new Yaml()
  val yamlSource = Source.fromFile("config.yaml")
  val yamlString = yamlSource.getLines().mkString("\n")

  val ret =
    yaml.load(yamlString).asInstanceOf[java.util.LinkedHashMap[String, Any]]

  println(ret.get("allowSynth").getClass)
  println(ret.get("allowSynth"))
  println(ret.get("allowSynth").asInstanceOf[Boolean])
  println(ret.get("verbose").getClass)
  println(ret.get("verbose"))
}
