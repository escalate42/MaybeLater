import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "wert_lex"
  val buildVersion      = "0.1-SNAPHSOT"
  val buildScalaVersion = "2.10.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt,
    scalacOptions ++= Seq("-unchecked", "-deprecation")
  )
}


object MLBuild extends Build {
	import Dependencies._
	import BuildSettings._

	lazy val maybeLater = Project (
		"maybeLater",
		file("."),
		settings = buildSettings
	)
}
