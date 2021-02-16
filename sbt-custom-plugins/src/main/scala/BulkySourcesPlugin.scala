package main.scala

import sbt._
import Keys._

object BulkySourcesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  val bulkyThresholdInLines = settingKey[Int]("Threshold with number of lines")
  val bulkySources = taskKey[Seq[(Int, File)]]("Sequence of source code files with number of lines")
  // T in SettingKey[T] indicates the type of value a setting has, T in TaskKey[T] indicates the type of the task’s result


  def getBulkyLines(sourceFilesSources: Seq[File], threshold: Int): Seq[(Int, File)] = sourceFilesSources
    .map(file => (sbt.IO.readLines(file).length, file))
    .filter { case (len, _) => len >= threshold }
    .sortBy { case (len, _) => -len }

  override val projectSettings: Seq[Setting[_]] = Seq(
    bulkyThresholdInLines := 100,

    bulkySources := getBulkyLines((Compile / sources).value, bulkyThresholdInLines.value),
    (Test / bulkySources) := getBulkyLines((Test / sources).value, bulkyThresholdInLines.value)
  )
}