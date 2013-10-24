package wertlex

import scala.concurrent._

/**
 * User: wert
 * Date: 24.10.13
 * Time: 19:27
 */
package object tools {

  /** Starts an asynchronous computation and returns a `MaybeLater` object with the result of that computation.
    *
    *  The result becomes available once the asynchronous computation is completed.
    *
    *  @tparam A       the type of the result
    *  @param body     the asynchronous computation
    *  @param ec       the execution context on which the future is run
    *  @return         the `MaybeLater` holding the result of the computation
    */
  def maybeLater[A](body: => Option[A])(implicit ec: ExecutionContext): MaybeLater[A] = MaybeLater(future(body))

}
