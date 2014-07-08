package com.github.wertlex.tools

import scala.collection.generic.CanBuildFrom
import scala.concurrent.ExecutionContext
import scalaz._
import scalaz.concurrent.Task

/**
 * Created by dns1 on 05.07.2014.
 */
package object eitherlater {

  type EitherLater[+L, +R] = EitherT[Task, L, R]

  def sequence[L, R, M[_] <: TraversableOnce[_]](in: M[EitherLater[L, R]])(
    implicit cbf: CanBuildFrom[M[EitherLater[L, R]], R, M[R]], executor: ExecutionContext
  ): EitherLater[L, M[R]] = {
    in.foldLeft(eitherLaterRight(cbf(in))) {
      (fr, fa) => fr.flatMap { r =>
        EitherT(
          fa.asInstanceOf[EitherLater[L, R]].run.map {
            case \/-(a)  => \/-(r += a)
            case -\/(err) => \/-(r)
          }
        )
      }
    } map (_.result())
  }

  def eitherLater[L, R](body: => Either[L, R]): EitherLater[L, R] = EitherT.fromEither(Task(body))

  def eitherLater[L, R](left: L)(body: => Option[R]): EitherLater[L, R] = EitherT.fromEither(
    Task(body).map {
      case None     => Left(left)
      case Some(v)  => Right(v)
    }
  )

  def eitherLaterLeft[L, R](left: L): EitherLater[L, R] = EitherT.left(Task.now(left))

  def eitherLaterRight[L, R](body: => R): EitherLater[L, R] = EitherT.right(Task(body))

  def el[L, R](body: => Either[L, R]): EitherLater[L, R] = eitherLater(body)

  def el[L, R](left: L)(body: => Option[R]): EitherLater[L, R] = eitherLater(left)(body)

  def ell[L, R](left: L): EitherLater[L, R] = eitherLaterLeft(left)

  def elr[L, R](body: => R): EitherLater[L, R] = eitherLaterRight(body)
}
