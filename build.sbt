import sbt.Keys.unmanagedJars
import sbt.file

ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.12.16"
ThisBuild / organization := "org.chainsaw"

// SpinalHDL
val spinalVersion = "1.7.3"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

// JGraphT JDK=1.8
val jGraphTVersion = "1.4.0" // last version compatible with Java 1.8
val jGraphCore = "org.jgrapht" % "jgrapht-core" % jGraphTVersion
val jGraphExt = "org.jgrapht" % "jgrapht-ext" % jGraphTVersion

// optimus
val optimusVersion = "3.2.4"
val optimus = "com.github.vagmcs" %% "optimus" % optimusVersion
val optimusOj = "com.github.vagmcs" %% "optimus-solver-oj" % optimusVersion
val optimusLp = "com.github.vagmcs" %% "optimus-solver-lp" % optimusVersion

lazy val Chainsaw = (project in file("."))
  .settings(
    name := "Chainsaw",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin),

    libraryDependencies ++= Seq(jGraphCore, jGraphExt),

    libraryDependencies += "org.json4s" %% "json4s-jackson" % "4.0.3", // for json serialization/deserialization
    libraryDependencies += "org.scalanlp" %% "breeze" % "1.0", // for numeric & matrix operations
    libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.7", // for finite field operations

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9", // for scala test
    libraryDependencies ++= Seq(optimus, optimusOj, optimusLp),
    Compile / unmanagedJars += file("lib/cplex.jar"),
    Compile / unmanagedJars += file("lib/engine.jar")
  )


fork := true