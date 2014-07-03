package com.github.wertlex.tools.maybelater

import org.specs2.mutable._
import org.specs2.time.NoTimeConversions
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.atomic.AtomicLong
import org.specs2.specification.Scope

/**
 * User: wert
 * Date: 22.10.13
 * Time: 23:41
 */
class MaybeLaterTest extends Specification with NoTimeConversions {

  private implicit val ec = scala.concurrent.ExecutionContext.global

  "MaybeLater in general" should {
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
      val ml = maybeLater {
        Thread.sleep(3000)
        Some("text")
      }
      Await.result(ml, 1 second) must throwA[TimeoutException]
    }

    "successfully Await.result for long running process" in {
      val ml = maybeLater {
        Thread.sleep(2000)
        Some("text")
      }
      Await.result(ml, 3 seconds) must beEqualTo("text")
    }


    "implement both Awaitable[A] and Awaitable[Option[A]]" in {
      val ml = maybeLater(Some("text"))
      val asA:    String          = Await.result(ml, 1 second)
      val asOptA: Option[String]  = Await.result(ml.asAwaitableOpt, 1 second)

      (asA must beEqualTo("text")) and
      (asOptA must beEqualTo(Some("text")))
    }

  }

  "MaybeLater.map" should {
    "apply map function once to contained value in synchronous mode" in new CounterAndFunc {
      val ml = MaybeLater.nowSome("Text").map(func)
      Await.result(ml, 1 second) must beEqualTo("TextText") and (
        counter.get must beEqualTo(1L)
        )
    }

    "apply map function once to contained value in asynchronous mode" in new CounterAndFunc {
      val ml = maybeLater{
        Thread.sleep(2000)
        Some("Text")
      }.map(func)
      Await.result(ml, 3 seconds) must beEqualTo("TextText") and (
        counter.get must beEqualTo(1L)
        )
    }

    "do not apply map function if no contained value" in new CounterAndFunc {
      val ml = maybeLater{
        Thread.sleep(2000)
        None
      }.map(func)
      Await.result(ml, 3 seconds) must throwA[NoSuchElementException] and (
        counter.get must beEqualTo(0L)
        )
    }

    "throw an exception on exception in map function" in new CounterAndFunc {
      val ml = maybeLater {
        Thread.sleep(1000)
        throw new Exception("Wow, thrown!")
        Some("text")
      }
      Await.result(ml.map(func), 3 seconds) must throwA[Exception]
    }
  }

  "MaybeLater.fold" should {
    "fold to value of one type in any case" in {
      val fullML  = MaybeLater.nowSome("text")
      val emptyML = MaybeLater.nowNone

      val m1 = fullML.fold("empty", _ => "full")
      val m2 = emptyML.fold("empty", _ => "full")

      Await.result(m1, 10 seconds) must beEqualTo("full")
      Await.result(m2, 10 seconds) must beEqualTo("empty")
    }
  }

  "MaybeLater.flatFold" should {
    "fold to value of one type in any case" in {
      val fullML = MaybeLater.nowSome[String]("text")
      val emptyML = MaybeLater.nowNone[String]

      val m1 = fullML.flatFold(MaybeLater.nowSome(false))(_ => MaybeLater.nowSome(true))
      val m2 = emptyML.flatFold(MaybeLater.nowSome(false))(_ => MaybeLater.nowSome(true))

      Await.result(m1, 10 seconds) must beEqualTo(true)
      Await.result(m2, 10 seconds) must beEqualTo(false)
    }
  }

  "MaybeLater[Option[A]]" should {
    "be squashable to MaybeLater[A]" in {
      val ml = MaybeLater.nowSome(Option("text"))
      val squashedML = ml.squash
      true
    }
  }


  "maybeLater" should {
    "construct MaybeLater from Option[A]" in {
      val ml = maybeLater { Some("text") }
      Await.result(ml, 10 seconds) must beEqualTo("text")
    }
  }


}


trait CounterAndFunc extends Scope {
  val counter = new AtomicLong(0)
  def func(s: String) = {
    counter.incrementAndGet()
    s"$s$s"
  }
}