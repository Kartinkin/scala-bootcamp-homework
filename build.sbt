lazy val root = (project in file("."))
  .aggregate(bulkySourcesPlugin)
  .settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.13.3"
    )),
    name := "bootcamp-homework"
  )
  .dependsOn(bulkySourcesPlugin)
  .enablePlugins(BulkySourcesPlugin)

lazy val bulkySourcesPlugin = Project("bulkySources", file("bulkySources"))

val catsScalacheckVersion = "0.2.0"
val scalaTestVersion = "3.1.0.0-RC2"
val catsVersion = "2.2.0"
val catsTaglessVersion = "0.11"
val catsEffectVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.scalatest" %% "scalatest" % "3.2.2" % Test,
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)

lazy val dependenciesDiff = taskKey[Classpath]("dependencies diff between Compile and Test configs")
dependenciesDiff := Def.task {
    val testJars = (Test / fullClasspathAsJars).value
    val compileJars = (Compile / fullClasspathAsJars).value
    testJars diff compileJars
  }.value
