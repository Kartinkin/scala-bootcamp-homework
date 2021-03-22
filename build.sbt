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
val catsVersion = "2.2.0"
val catsTaglessVersion = "0.11"
val catsEffectVersion = "2.2.0"
val circeVersion = "0.13.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  // "org.scalatest" %% "scalatest" % "3.2.2" % Test,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % "2.4.2" % Test,
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test)

scalacOptions ++= Seq(
  "-Ymacro-annotations"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)

lazy val dependenciesDiff = taskKey[Classpath]("dependencies diff between Compile and Test configs")
dependenciesDiff := Def.task {
    val testJars = (Test / fullClasspathAsJars).value
    val compileJars = (Compile / fullClasspathAsJars).value
    testJars diff compileJars
  }.value
