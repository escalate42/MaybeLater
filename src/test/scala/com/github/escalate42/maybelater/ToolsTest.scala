package com.github.escalate42.maybelater

import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions

import scala.concurrent._
import scala.concurrent.duration._

/**
 * User: wert
 * Date: 18.12.13
 * Time: 22:13
 */
class ToolsTest extends Specification with NoTimeConversions {

  import com.github.escalate42.maybelater.Tools._

import scala.concurrent.ExecutionContext.Implicits.global

  "Tools" should {
    "allow ml.toFutureBoolean syntax for MaybeLater[Boolean]" in {

      val ml = MaybeLater.nowSome(true)
      ml.toFutureBoolean // this should be compiled well
      true
    }

    "return boolean as is if some boolean presented, or false if no boolean found" in {

      Await.result( MaybeLater.nowSome(true).toFutureBoolean, 10 seconds)                   == true and
      Await.result( MaybeLater.nowSome(false).toFutureBoolean, 10 seconds)                  == false and
      Await.result( (MaybeLater.nowNone: MaybeLater[Boolean]).toFutureBoolean, 10 seconds)  == false
    }

    "allow .toMaybeLater syntax for Future[Option[T]]" in {
      val ml: MaybeLater[String] = Future.successful(Option("text")).toMaybeLater // it should be compiled well
      true
    }

    "allow .flatten syntax for MaybeLater[Option[T]]" in {
      Await.result(MaybeLater.nowSome[Option[String]](Some("true")).flatten.asAwaitableOpt, 10 seconds) == Some("true") and
      Await.result(MaybeLater.nowSome[Option[String]](None).flatten.asAwaitableOpt, 10 seconds) == None and
      Await.result(MaybeLater.nowNone[Option[String]].flatten.asAwaitableOpt, 10 seconds) == None
    }

    "allow .toMaybeLater syntax on List[MaybeLater[T]]" in {
      val mlList: List[MaybeLater[Int]] = List(
        MaybeLater.nowSome(1), MaybeLater.nowNone, MaybeLater.nowSome(3), MaybeLater.nowSome(4)
      )
      Await.result(mlList.toMaybeLater.asAwaitable, 10 seconds) == List(1, 3, 4)
    }

    "allow .toMaybeLater syntax on Future[List[T]]" in {
      val list = List(1, 2, 3, 4, 5)
      val fList = Future { List(1, 2, 3, 4, 5) }
      Await.result(fList.toMaybeLater.asAwaitable, 10 seconds) == list
    }
  }
}
