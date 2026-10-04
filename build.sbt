ThisBuild / scalaVersion := "3.9.0"
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
    publish / skip := true,
    Compile / run / mainClass := Some("Example"),
    Compile / run / javaOptions ++= Seq("-Xms512M", "-Xmx4G"),
    Compile / scalacOptions ++= Seq(
      "-Xplugin:" + (plugin / Compile / packageBin).value.getAbsolutePath,
      "-Xplugin-require:use-quotation",
      "-Vprint:staging,quotationElaboration"
    ),
    Test / fork := true,
    Test / parallelExecution := false,
    Test / javaOptions ++= Seq("-Xms512M", "-Xmx4G"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test
  )
