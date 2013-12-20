package wertlex.tools

import org.specs2.mutable.Specification
import com.github.wertlex.tools.maybelater.MaybeLater
import scala.concurrent._
import scala.concurrent.duration._
import org.specs2.time.NoTimeConversions
import com.github.wertlex.tools.maybelater.Tools._

/**
 * User: wert
 * Date: 18.12.13
 * Time: 22:13
 */
class Tools extends Specification with NoTimeConversions {

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
  }
}
