package com.github.wertlex.tools.eitherlater

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Failure, Success}
import scalaz._
import scalaz.concurrent.Task

/**
 * Created by dns1 on 05.07.2014.
 */
object Tools {

  /**
   * Implicit class to provide maybeLater.flatten syntax
   * @param el
   */
  implicit class ListOfEitherLaterFlatten[L, R, M[_] <: TraversableOnce[_]](val el: M[EitherLater[L, R]]) extends AnyVal {
    def toEitherLater(
      implicit cbf: CanBuildFrom[M[EitherLater[L, R]], R, M[R]],
      executor: ExecutionContext
    ): EitherLater[L, M[R]] = sequence(el)
  }

  /**
   * Implicit class to provide maybeLater.flatten syntax
   * @param el
   */
  implicit class EitherLaterOptionFlatten[L, R](val el: EitherLater[L, Option[R]]) extends AnyVal {
    def flatten(left: L)(implicit ec: ExecutionContext): EitherLater[L, R] = EitherT(
      el.run.map {
        case -\/(l) => -\/(l)
        case \/-(o) => o match {
          case Some(v) => \/-(v)
          case None    => -\/(left)
        }
      }
    )
  }

  /**
   * Implicit class to provide maybeLater.toFutureBoolean syntax
   * @param el
   */
  implicit class SimplifyToTask[L](val el: EitherLater[L, Boolean]) extends AnyVal {
    def toTaskBoolean(implicit ec: ExecutionContext): Task[Boolean] = el.run.map {
      case \/-(value) => value
      case -\/(_)     => false
    }
  }

  /**
   * Implicit class to provide maybeLater.toFutureBoolean syntax
   * @param el
   */
  implicit class SimplifyToFuture[L](val el: EitherLater[L, Boolean]) extends AnyVal {
    def toFutureBoolean(implicit ec: ExecutionContext): Future[Boolean] = scalazTaskToScalaFuture(
      el.run.map {
        case \/-(value) => value
        case -\/(_)     => false
      }
    )
  }

  /**
   * Allows myFuture.toMaybeLater syntax for future
   * @param fo - future
   * @tparam R - right param
   */
  implicit class FutureOptionToEitherLater[R](val fo: Future[Option[R]]) extends AnyVal {
    def toEitherLater[L](left: L)(implicit ec: ExecutionContext): EitherLater[L, R] = EitherT(
      scalaFutureToScalazTask(fo).map {
        case Some(v) => \/-(v)
        case None    => -\/(left)
      }
    )
  }

  /**
   * Allows myFuture.toMaybeLater syntax for future
   * @param fo - future
   * @tparam R - right type
   */
  implicit class FutureListToEitherLater[R](val fo: Future[List[R]]) extends AnyVal {
    def toEitherLater[L](implicit ec: ExecutionContext): EitherLater[L, List[R]] = EitherT.right(
      scalaFutureToScalazTask(fo)
    )
  }

  def scalaFutureToScalazTask[T](f: Future[T])(implicit ec: ExecutionContext): Task[T] = Task.async[T]( reg =>
    f.onComplete {
      case Success(t)   => reg(\/.right(t))
      case Failure(exc) => reg(\/.left(exc))
    }
  )

  implicit class ScalaFutureToScalazTask[T](val f: Future[T]) extends AnyVal {
    def toScalazTask(implicit ec: ExecutionContext) = scalaFutureToScalazTask(f)
  }

  def scalazTaskToScalaFuture[T](t: Task[T])(implicit ec: ExecutionContext): Future[T] = t.attemptRun match {
    case \/-(value) => Future.apply(value)
    case -\/(exc)   => Future.failed(exc)
  }

  implicit class ScalazTaskToScalaFuture[T](val t: Task[T]) extends AnyVal {
    def toScalaFuture(implicit ec: ExecutionContext) = scalazTaskToScalaFuture(t)
  }
}
