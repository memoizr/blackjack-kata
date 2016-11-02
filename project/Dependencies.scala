import sbt._
import Keys._

object Dependencies extends Build {
  def scalatest = "org.scalatest" %% "scalatest" % "2.2.4" % "test"
}

