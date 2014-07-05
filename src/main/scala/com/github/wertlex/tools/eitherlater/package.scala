package com.github.wertlex.tools

import scala.collection.generic.CanBuildFrom
import scala.concurrent.ExecutionContext
import scalaz._
import scalaz.concurrent.Task

/**
 * Created by dns1 on 05.07.2014.
 */
package object eitherlater {
  case class LeftDescription(value: String)

  implicit def leftDescriptionEquals = Equal.equalA[LeftDescription]

  type EitherLater[A] = EitherT[Task, LeftDescription, A]

  def sequence[A, M[_] <: TraversableOnce[_]](in: M[EitherLater[A]])(
    implicit cbf: CanBuildFrom[M[EitherLater[A]], A, M[A]], executor: ExecutionContext
  ): EitherLater[M[A]] = {
    in.foldLeft(eitherLaterRight(cbf(in))) {
      (fr, fa) => fr.flatMap { r =>
        EitherT(
          fa.asInstanceOf[EitherLater[A]].run.map {
            case \/-(a)  => \/-(r += a)
            case -\/(err) => \/-(r)
          }
        )
      }
    } map (_.result())
  }

  def eitherLater[A](body: => Either[LeftDescription, A]): EitherLater[A] = EitherT.fromEither(Task(body))

  def eitherLater[A](left: LeftDescription)(body: => Option[A]): EitherLater[A] = EitherT.fromEither(
    Task(body).map {
      case None     => Left(left)
      case Some(v)  => Right(v)
    }
  )

  def eitherLaterLeft[A](left: LeftDescription): EitherLater[A] = EitherT.left(Task.now(left))

  def eitherLaterRight[A](body: A): EitherLater[A] = EitherT.right(Task(body))

  def el[A](body: => Either[LeftDescription, A]): EitherLater[A] = eitherLater(body)

  def el[A](left: LeftDescription)(body: => Option[A]): EitherLater[A] = eitherLater(left)(body)

  def ell[A](left: LeftDescription): EitherLater[A] = eitherLaterLeft(left)

  def elr[A](body: A): EitherLater[A] = eitherLaterRight(body)
}
