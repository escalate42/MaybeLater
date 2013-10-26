package wertlex.tools

import scala.concurrent._
import scala.Some
import scala.util.{Success, Failure, Try}
import scala.util.control.NonFatal
import scala.concurrent.duration.Duration

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
class MaybeLater[A](val body: Future[Option[A]]) {

  def map[B](f: A => B)(implicit ec: ExecutionContext): MaybeLater[B] =  new MaybeLater(
    body.map{ maybeA => maybeA.map(f) }
  )

  def flatMap[B](f: A => MaybeLater[B])(implicit ec: ExecutionContext): MaybeLater[B] = {
    val pr = promise[Option[B]]
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

  implicit def toAwaitable[A](ml: MaybeLater[A]) = ml.asAwaitable
}


/**
 * Wrapper which implements Awaitable[A] interface for MaybeLater[A]
 * @param ml MaybeLater to extends with Awaitable[A] interface
 * @tparam A
 */
class MaybeLaterAwaitable[A](private val ml: MaybeLater[A]) extends Awaitable[A] {

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    ml.body.ready(atMost)
    this
  }

  def result(atMost: Duration)(implicit permit: CanAwait): A = {
    ml.body.result(atMost).get
  }
}

/**
 * Wrapper which implements Awaitable[A] interface for MaybeLater[Option[A]]
 * @param ml MaybeLater to extends with Awaitable[Option[A]] interface
 * @tparam A
 */
class MaybeLaterAwaitableOption[A](private val ml: MaybeLater[A]) extends Awaitable[Option[A]] {

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    ml.body.ready(atMost)
    this
  }

  def result(atMost: Duration)(implicit permit: CanAwait): Option[A] = {
    ml.body.result(atMost)
  }

}
