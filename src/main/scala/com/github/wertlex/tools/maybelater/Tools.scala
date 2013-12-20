package com.github.wertlex.tools.maybelater

import scala.concurrent.{ExecutionContext, Future}

/**
 * User: wert
 * Date: 18.12.13
 * Time: 22:04
 */
object Tools {

  /**
   * Implicit class to provide maybeLater.toFutureBoolean syntax
   * @param ml
   */
  implicit class SimplifyToFuture(val ml: MaybeLater[Boolean]) {

    def toFutureBoolean(implicit ec: ExecutionContext): Future[Boolean] = ml.asFuture.map{ optRes =>
        optRes match {
          case Some(value) => value
          case None => false
        }
      }

  }

  /**
   * Allows myFuture.toMaybeLater syntax for future
   * @param fo
   * @tparam T
   */
  implicit class FutureOptionToMaybeLater[T](val fo: Future[Option[T]]) {
    def toMaybeLater: MaybeLater[T] = MaybeLater(fo)
  }
}

