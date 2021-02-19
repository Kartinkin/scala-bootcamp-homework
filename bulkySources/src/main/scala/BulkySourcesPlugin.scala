import sbt._
import Keys._

object BulkySourcesPlugin extends AutoPlugin {

  import autoImport._

  object autoImport {
    lazy val bulkyThresholdInLines= settingKey[Int]("Bulky threshold (default: 100)")
    lazy val bulkySources = taskKey[Seq[(Int, File)]]("Returns list of source files longer than bulky threshold")
  }

  override lazy val globalSettings: Seq[Setting[_]] = Seq(
    bulkyThresholdInLines := 100
  )

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    (Compile / bulkySources) := getBulkySources((sources in Compile).value, bulkyThresholdInLines.value),
    (Test / bulkySources) := getBulkySources((sources in Test).value, bulkyThresholdInLines.value)
  )

  def getBulkySources(files: Seq[File], threshold: Int): Seq[(Int, sbt.File)] =
    (for {
      file <- files
      lines = sbt.IO.readLines(file).size
      if lines >= threshold
    } yield (lines, file))
    .sortBy { case (lines, _) => lines }
    .reverse

  }
