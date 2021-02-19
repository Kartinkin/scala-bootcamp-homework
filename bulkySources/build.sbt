
lazy val bulkySources = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "bulky-sources-plugin",
    version := "0.0",
    scalaVersion := "2.12.12"
  )
