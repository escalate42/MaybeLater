import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "escalate42"
  val buildVersion      = "0.1-SNAPHSOT"
  val buildScalaVersion = "2.11.4"

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
		settings = buildSettings ++ Seq(libraryDependencies ++= test.deps)
	)
}
