ThisBuild / scalaVersion := "3.7.4"
ThisBuild / organization := "use-quotation"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = project.in(file("."))
  .aggregate(runtime, plugin, example)
  .settings(
    name := "use-quotation",
    publish / skip := true
  )

lazy val plugin = project.in(file("plugin"))
  .settings(
    name := "use-quotation-plugin",
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Provided
  )

lazy val runtime = project.in(file("runtime"))
  .settings(
    name := "use-quotation-runtime",
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value
  )

lazy val example = project.in(file("example"))
  .dependsOn(runtime)
  .settings(
    name := "use-quotation-example",
    Compile / run / mainClass := Some("Example"),
    publish / skip := true,
    Compile / scalacOptions ++= Seq(
      "-Xplugin:" + (plugin / Compile / packageBin).value.getAbsolutePath,
      "-Xplugin-require:use-quotation",
      "-Xprint:staging,quotationElaboration"
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test
  )
