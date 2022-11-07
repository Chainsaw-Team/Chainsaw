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

lazy val Chainsaw = (project in file("."))
  .settings(
    name := "Chainsaw",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin),
    libraryDependencies ++= Seq(jGraphCore, jGraphExt)
  )

fork := true
