lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "sbt-custom-plugins",
    organization := "sbt.plugins",
    version := "0.1"
  )