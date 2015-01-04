package com.github.escalate42.maybelater

import scala.concurrent.{ExecutionContext, Future}
import scalaz._
import Scalaz._

/**
 * Right-biased wrapper over Future[ReasonT \/ SuccessT]
 * User: wert
 * Date: 02.01.15
 * Time: 21:54
 */
class LaterEither[ReasonT, SuccessT](private val plain: Future[ReasonT \/ SuccessT]) {

  def map[B](f: SuccessT => B)(implicit ec: ExecutionContext): LaterEither[ReasonT, B] = {
    val r = plain.map{ either =>
      either.map(f)
    }
    new LaterEither(r)
  }

  def flatMap[B](f: SuccessT => LaterEither[ReasonT, B])(implicit ec: ExecutionContext): LaterEither[ReasonT, B] = {
    val r: Future[ReasonT \/ B] = plain.flatMap{ either =>
     either match {
       case -\/(l) => Future.successful(-\/(l))
       case \/-(r) => f(r).plain
     }
    }
    new LaterEither(r)
  }

  def leftMap[A](f: ReasonT => A)(implicit ec: ExecutionContext): LaterEither[A, SuccessT] = {
    val r = plain.map{ either =>
      either.leftMap(f)
    }

    new LaterEither(r)
  }


  def leftFlatMap[A](f: ReasonT => LaterEither[A, SuccessT])(implicit ec: ExecutionContext): LaterEither[A, SuccessT] = {
    val r = plain.flatMap{ either =>
      either match {
        case -\/(l) => f(l).plain
        case \/-(r) => Future.successful(\/-(r))
      }
    }
    new LaterEither(r)
  }

  def swap()(implicit ec: ExecutionContext): LaterEither[SuccessT, ReasonT] = {
    val r = plain.map{ either =>
      either.swap
    }
    new LaterEither(r)
  }

  def transform[AnotherReasonT, AnotherSuccessT](reasonF: ReasonT => AnotherReasonT, successF: SuccessT => AnotherSuccessT)(implicit ec: ExecutionContext): LaterEither[AnotherReasonT, AnotherSuccessT] = {
    val r = plain.map{ either =>
      either match {
        case -\/(l) => -\/(reasonF(l))
        case \/-(r) => \/-(successF(r))
      }
    }
    new LaterEither(r)
  }
}


object LaterEither {
  def apply[ReasonT, SuccessT](plain: Future[ReasonT \/ SuccessT]): LaterEither[ReasonT, SuccessT] = {
    new LaterEither(plain)
  }

  def apply[ReasonT, SuccessT](plain: Future[Either[ReasonT, SuccessT]])(implicit ec: ExecutionContext): LaterEither[ReasonT, SuccessT] = {
    apply(plain.map( \/.fromEither _ ))
  }

  def apply[ReasonT, SuccessT](plainOption: Future[Option[SuccessT]], onEmpty: => ReasonT)(implicit ec: ExecutionContext): LaterEither[ReasonT, SuccessT] = {
    val r = plainOption.map{ opt =>
      opt match {
        case Some(x)    => \/-(x)
        case None       => -\/(onEmpty)
      }
    }
    new LaterEither(r)
  }



  object Implicits {

    implicit class FutureScalazEitherOps[A, B](val value: Future[A \/ B]) extends AnyVal {
      def toLaterEither = apply(value)
    }

    implicit class FutureScalaEitherOps[A, B](val value: Future[Either[A, B]]) extends AnyVal {
      def toLaterEither(implicit ec: ExecutionContext) = apply(value)
    }

    implicit class FutureOptionEitherOps[A, B](val value: Future[Option[B]]) extends AnyVal {
      def toLaterEither(onEmpty: => A)(implicit ec: ExecutionContext) = apply(value, onEmpty)
    }
  }
}


object Test {

  import LaterEither.Implicits._
  import scala.concurrent.ExecutionContext.Implicits.global


  case class Error(descr: String)
  case class NotFound(descr: String)
  case class User(name: String)
  case class BSON(value: String)


  val fes: Future[Error \/ String] = ???
  val le: LaterEither[Error, User] = ???


  def fromBSONCollection: Future[Option[BSON]] = Future.successful(Some(BSON("")))


  val r = fromBSONCollection.toLaterEither(onEmpty = NotFound("no such bson"))


}