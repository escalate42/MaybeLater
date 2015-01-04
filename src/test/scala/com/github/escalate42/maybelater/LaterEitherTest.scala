package com.github.escalate42.maybelater

import org.specs2.mutable._
import scalaz._
import scala.concurrent._

/**
 * User: wert
 * Date: 04.01.15
 * Time: 13:02
 */
class LaterEitherTest extends Specification {

  case class Result(info: String)
  case class Reason(info: String)

  "LaterEither" should {
    """wrap up Future[A \/ B]""" in {
      val either: Reason \/ Result = \/-(Result("here is result"))
      val le = LaterEither(Future.successful(either))

      le must not beNull
    }

    """wrap up Future[Either[A, B]]""" in {
      val either: Either[Reason, Result] = Right(Result("here is result"))
      val le = LaterEither(Future.successful(either))

      le must not beNull
    }


  }
}
