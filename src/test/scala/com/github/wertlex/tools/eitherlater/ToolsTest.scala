package com.github.wertlex.tools.eitherlater

import com.github.wertlex.tools.eitherlater.Tools._
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import scala.concurrent._
import scalaz._
import scalaz.std.AllInstances._

/**
 * Created by dns1 on 05.07.2014.
 */
class ToolsTest extends Specification with NoTimeConversions {

  case class LeftDescription(value: String)
  implicit def leftDescriptionEquals = Equal.equalA[LeftDescription]
  val left = LeftDescription("fail")

  "Tools" should {
    "allow ml.toTaskBoolean syntax for MaybeLater[Boolean]" in {
      eitherLaterRight(true).toTaskBoolean.run
    }

    "return boolean as is if some boolean presented, or false if no boolean found" in {
      eitherLaterRight(true).toTaskBoolean.run  === true  and
      eitherLaterRight(false).toTaskBoolean.run === false and
      eitherLaterLeft(left).toTaskBoolean.run === false
    }

    "allow .toEitherLater syntax for Future[Option[T]]" in {
      Future.successful(Option("text")).toEitherLater(left).run.run === \/-("text")
    }

    "allow .flatten syntax for EitherLater[Option[T]]" in {
      eitherLaterRight[LeftDescription, Option[String]](Some("true")).flatten(left).run.run === \/-("true") and
      eitherLaterRight[LeftDescription, Option[String]](None).flatten(left).run.run === -\/(left) and
      eitherLaterLeft[LeftDescription, Option[String]](left).flatten(left).run.run === -\/(left)
    }

    "allow .toEitherLater syntax on List[EitherLater[T]]" in {
      val elList: List[EitherLater[LeftDescription, Int]] = List(elr(1), ell(left), elr(3), elr(4))
      elList.toEitherLater.run.run === \/-(List(1, 3, 4))
    }

    "allow .toEitherLater syntax on Future[List[T]]" in {
      val list = List(1, 2, 3, 4, 5)
      val fList = future {
        List(1, 2, 3, 4, 5)
      }
      fList.toEitherLater[LeftDescription].run.run === \/-(list)
    }

    "allow to use EitherT methods on EitherLater" in {
      ell[LeftDescription, Int](left).isLeft.run
    }
  }
}