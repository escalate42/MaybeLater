package wertlex.tools

import org.specs2.mutable.Specification
import com.github.wertlex.tools.maybelater.MaybeLater
import scala.concurrent.Await
import scala.concurrent.duration._
import org.specs2.time.NoTimeConversions

/**
 * User: wert
 * Date: 18.12.13
 * Time: 22:13
 */
class Tools extends Specification with NoTimeConversions {

  "Tools" should {
    "allow ml.toFutureBoolean syntax for MaybeLater[Boolean]" in {
      import com.github.wertlex.tools.maybelater.Tools._
      import scala.concurrent.ExecutionContext.Implicits.global
      val ml = MaybeLater.nowSome(true)
      ml.toFutureBoolean // this should be compiled well
      true
    }

    "return boolean as is if some boolean presented, or false if no boolean found" in {
      import com.github.wertlex.tools.maybelater.Tools._
      import scala.concurrent.ExecutionContext.Implicits.global
      Await.result( MaybeLater.nowSome(true).toFutureBoolean, 10 seconds)                   == true and
      Await.result( MaybeLater.nowSome(false).toFutureBoolean, 10 seconds)                  == false and
      Await.result( (MaybeLater.nowNone: MaybeLater[Boolean]).toFutureBoolean, 10 seconds)  == false
    }
  }
}
