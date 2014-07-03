package com.github.wertlex.tools.maybelater

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scalaz.concurrent.Task
import scalaz.OptionT
import scalaz.OptionT._
import scalaz.{\/, \/-, -\/}

/**
 * User: wert
 * Date: 18.12.13
 * Time: 22:04
 */
object Tools {

  /**
   * Implicit class to provide maybeLater.flatten syntax
   * @param ml
   */
  implicit class ListOfMaybeLaterFlatten[A, M[_] <: TraversableOnce[_]](val ml: M[MaybeLater[A]]) extends AnyVal {

    def toMaybeLater(implicit cbf: CanBuildFrom[M[MaybeLater[A]], A, M[A]], executor: ExecutionContext): MaybeLater[M[A]] =
      MaybeLater.sequence(ml)

  }

  /**
   * Implicit class to provide maybeLater.flatten syntax
   * @param ml
   */
  implicit class MaybeLaterOptionFlatten[A](val ml: MaybeLater[Option[A]]) extends AnyVal {

    def flatten(implicit ec: ExecutionContext): MaybeLater[A] = ml.asFuture.map(_.flatten).toMaybeLater

  }

  /**
   * Implicit class to provide maybeLater.toFutureBoolean syntax
   * @param ml
   */
  implicit class SimplifyToFuture(val ml: MaybeLater[Boolean]) extends AnyVal {

    def toFutureBoolean(implicit ec: ExecutionContext): Future[Boolean] = ml.asFuture.map {
      case Some(value) => value
      case None => false
    }

  }

  /**
   * Allows myFuture.toMaybeLater syntax for future
   * @param fo
   * @tparam T
   */
  implicit class FutureOptionToMaybeLater[T](val fo: Future[Option[T]]) extends AnyVal {
    def toMaybeLater: MaybeLater[T] = MaybeLater(fo)
  }

  /**
   * Allows myFuture.toMaybeLater syntax for future
   * @param fo
   * @tparam T
   */
  implicit class FutureListToMaybeLater[T](val fo: Future[List[T]]) extends AnyVal {
    def toMaybeLater(implicit ec: ExecutionContext): MaybeLater[List[T]] = fo.map(Option(_)).toMaybeLater
  }

  implicit class MaybeLaterToOptionT[T](val ml: MaybeLater[T]) extends AnyVal {
    def toOptionT(implicit ec: ExecutionContext): OptionT[Task, T] = optionT(scalaz.scalaFutureToScalazTask(ml.asFuture))
  }

  implicit class OptionTToMaybeLater[T](val ot: OptionT[Task, T]) extends AnyVal {
    def toMaybeLater(implicit ec: ExecutionContext): MaybeLater[T] = MaybeLater(scalaz.scalazTaskToScalaFuture(ot.run))
  }

  object scalaz {
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
}

