package wertlex.tools

import org.specs2.mutable._
import org.specs2.time.NoTimeConversions
import scala.concurrent._
import scala.concurrent.duration._


/**
 * User: wert
 * Date: 22.10.13
 * Time: 23:41
 */
class MaybeLaterTest extends Specification with NoTimeConversions {

  private implicit val ec = scala.concurrent.ExecutionContext.global

  "MaybeLater" should {
    "allow to construct value from Option" in {
      // must compiled
      MaybeLater.now(Option("Created"))
      MaybeLater.now(None)
      MaybeLater.nowNone
      MaybeLater.nowSome("String")
      success
    }

    "allow to get result with Await.result " in {
      val ml = MaybeLater(future{ Some("text") })
      Await.result(ml, 10 seconds) must beEqualTo("text")
    }

    "throw exception if result will be obtained later then Await.result duration" in {
      val ml = MaybeLater( future {
        Thread.sleep(3000)
        Some("text")
      })
      Await.result(ml, 1 second) must throwA[TimeoutException]
    }

    "successfully Await.result for long running process" in {
      val ml = MaybeLater( future {
        Thread.sleep(2000)
        Some("text")
      })
      Await.result(ml, 3 seconds) must beEqualTo("text")
    }
  }
}
