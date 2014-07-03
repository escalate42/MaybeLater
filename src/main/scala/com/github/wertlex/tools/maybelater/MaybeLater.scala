package com.github.wertlex.tools.maybelater

import scala.concurrent._
import scala.util.{Success, Failure}
import scala.util.control.NonFatal
import scala.concurrent.duration.Duration
import scala.collection.generic.CanBuildFrom
import scalaz.concurrent.Task
import scalaz.OptionT
import scalaz.OptionT._

/**
 * Wrapper over Future[Option[A]].
 * Provides map, flatMap, foreach, withFilter methods.
 *
 * User: wert
 * Date: 28.08.13
 * Time: 20:04
 * v.dubs
 * Date: 16.09.13 22:20
 */
class MaybeLater[+A](protected val body: Future[Option[A]]) {

  /**
   * A bit nicer way for casting MaybeLater[A] to Future[Option[A]]
   */
  val asFuture: Future[Option[A]] = body

  def map[B](f: A => B)(implicit ec: ExecutionContext): MaybeLater[B] =  new MaybeLater(
    body.map{ maybeA => maybeA.map(f) }
  )

  def flatMap[B](f: A => MaybeLater[B])(implicit ec: ExecutionContext): MaybeLater[B] = {
    val pr = promise[Option[B]]()
    val fu = pr.future

    body onComplete {
      case f:Failure[_]    => pr failure f.exception  // quick returning. No need to perform other operations
      case Success(maybeA) => {                       // something good happened. Will proceed with nested Option
        maybeA match {
          case None => pr success None                // option is empty, setting result to None
          case Some(a) =>                             // option not empty. will apply provided function f()
            try {                                     // for the case if something go wrong..
              f(a).body.onComplete {                  // applying f() at last
                case f: Failure[_]    => pr failure f.exception // once again quick returning on error
                case Success(optA)    => pr success optA        // the only success variation
              }
            } catch {
              case NonFatal(e) => pr failure e        // fail it if something goes wrong
            }

        }
      }
    }

    new MaybeLater(fu)
  }


  def fold[B](onNone: => B, onSome: A => B)(implicit ec: ExecutionContext): MaybeLater[B] = {
    val p = promise[B]()
    body.onComplete{
      case Success(optA) => {
        if(optA.isDefined)  p.success(onSome(optA.get))
        else                p.success(onNone)
      }
      case Failure(e) => p.success(onNone)
    }

    new MaybeLater[B](p.future.map(Option(_)))
  }

  def flatFold[B](onNone: MaybeLater[B])(onSome: A => MaybeLater[B])(implicit ec: ExecutionContext): MaybeLater[B] = MaybeLater(
    body.flatMap {
      case None    => onNone.asFuture
      case Some(v) => onSome(v).asFuture
    }
  )

  def withFilter(f: A => Boolean)(implicit ec: ExecutionContext): MaybeLater[A] = new MaybeLater(
    body.map { maybeA =>
      maybeA.filter(f)
    }
  )

  def foreach[B](f: A => B)(implicit ec: ExecutionContext): Unit = map(f)

  /**
   * Casts this to Awaitable[A] interface
   */
  lazy val asAwaitable:     Awaitable[A]          = new MaybeLaterAwaitable(this)

  /**
   * Casts this to Awaitable[Option[A]] interface
   */
  lazy val asAwaitableOpt:  Awaitable[Option[A]]  = new MaybeLaterAwaitableOption[A](this)

}

object MaybeLater {
  type MaybeLaterAlias[A] = Future[Option[A]]
  implicit def toMaybeLater[A](futureOption: MaybeLaterAlias[A])     = apply(futureOption)
//  implicit def fromMaybeLater[A](maybeLater: MaybeLater[A])          = unapply(maybeLater)
  def apply[A](futureOption: MaybeLaterAlias[A]): MaybeLater[A]      = new MaybeLater(futureOption)
  def unapply[A](maybeLater: MaybeLater[A]):      MaybeLaterAlias[A] = maybeLater.body
  def now[A](optA: Option[A]):                    MaybeLater[A]      = new MaybeLater(Future.successful(optA))
  def nowSome[A](a: A):                           MaybeLater[A]      = now(Some(a))
  def nowNone[A]:                                 MaybeLater[A]      = now(None)
  def sequence[A, M[_] <: TraversableOnce[_]](in: M[MaybeLater[A]])(
    implicit cbf: CanBuildFrom[M[MaybeLater[A]], A, M[A]], executor: ExecutionContext
  ): MaybeLater[M[A]] = {
    in.foldLeft(MaybeLater.nowSome(cbf(in))) {
      (fr, fa) => fr.flatMap { r =>
        fa.asInstanceOf[MaybeLater[A]].asFuture.map {
          case Some(a) => Some(r += a)
          case None    => Some(r)
        }
      }
    } map (_.result())
  }

  implicit def toAwaitable[A](ml: MaybeLater[A]) = ml.asAwaitable

  implicit def toSquashable[A](ml: MaybeLater[Option[A]]) = new MaybeLaterSquashable[A](ml)

  implicit def toOptionT[A](ml: MaybeLater[A])(implicit ec: ExecutionContext) = optionT(Tools.scalaz.scalaFutureToScalazTask(ml.body))

  implicit def toMaybeLater[A](ot: OptionT[Task, A])(implicit ec: ExecutionContext) = MaybeLater(Tools.scalaz.scalazTaskToScalaFuture(ot.run))
}


/**
 * Wrapper which implements Awaitable[A] interface for MaybeLater[A]
 * @param ml MaybeLater to extends with Awaitable[A] interface
 * @tparam A
 */
class MaybeLaterAwaitable[A](private val ml: MaybeLater[A]) extends Awaitable[A] {

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    ml.asFuture.ready(atMost)
    this
  }

  def result(atMost: Duration)(implicit permit: CanAwait): A = {
    ml.asFuture.result(atMost).get
  }
}

/**
 * Wrapper which implements Awaitable[A] interface for MaybeLater[Option[A]]
 * @param ml MaybeLater to extends with Awaitable[Option[A]] interface
 * @tparam A
 */
class MaybeLaterAwaitableOption[A](private val ml: MaybeLater[A]) extends Awaitable[Option[A]] {

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    ml.asFuture.ready(atMost)
    this
  }

  def result(atMost: Duration)(implicit permit: CanAwait): Option[A] = {
    ml.asFuture.result(atMost)
  }

}

/**
 * Squashes MaybeLater[Option[A]] to MaybeLater[A]
 * @param ml
 * @tparam A
 */
class MaybeLaterSquashable[A](private val ml: MaybeLater[Option[A]]) {
  def squash(implicit ec: ExecutionContext): MaybeLater[A] = {
    val r = ml.asFuture.map{ optOptA => optOptA.flatten}
    MaybeLater(r)
  }
}
