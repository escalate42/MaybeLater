import sbt._
import Keys._

object Dependencies {
  object test {
    lazy val deps = Seq(specs2)
    lazy val specs2 = "org.specs2" %% "specs2" % "2.3.12" % "test"
  }
  object scalaz {
    lazy val deps = Seq(scalazCore, scalazContrib, scalazConcurrent)
    lazy val scalazCore = "org.scalaz" %% "scalaz-core" % "7.0.6"
    lazy val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % "7.0.6"
    lazy val scalazContrib = "org.typelevel" %% "scalaz-contrib-210"  % "0.1.5"
  }
}
