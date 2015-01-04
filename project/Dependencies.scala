import sbt._
import Keys._

object Dependencies {
  object test {
    lazy val deps = Seq(specs2)
    lazy val specs2 = "org.specs2" %% "specs2-core" % "2.4.15" % "test"
  }

  object scalaz {
    lazy val core = "org.scalaz" %% "scalaz-core" % "7.1.0"
    lazy val deps = Seq(core)
  }
}
