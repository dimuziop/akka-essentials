package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

/**
 * User: patricio
 * Date: 16/4/21
 * Time: 05:22
 */
class TimeAssertionSpec extends TestKit(
  ActorSystem("TimedAssertionSpec", ConfigFactory.load().getConfig("specialTimeAssertionsConfig"))
)
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll {

  override protected def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import part3testing.TimeAssertionSpec._

  "A worker actor" should {
    val workerActor = system.actorOf(Props[WorkerActor])

    "reply with thw meaning of life in a timely manner" in {
      within(500 millis, 1 second) {
        workerActor ! "work"
        expectMsg(WorkResult(42))
      }
    }

    "reply with valid work at a reasonable cadence" in {
      within(1 second) {
        workerActor ! "workSequence"
        val results: Seq[Int] = receiveWhile[Int](max = 2 seconds, idle = 500 millis, messages = 10) {
          case WorkResult(result) => result
        }

        assert(results.sum > 5)

      }
    }

    "rely to a test probe in a timely manner" in {
      within(300 millis) {
        val probe = TestProbe()
        probe.send(workerActor, "work")
        probe.expectMsg(WorkResult(42)) // timeout with 0.3
      }
    }

  }

}

object TimeAssertionSpec {

  case class WorkResult(i: Int)

  class WorkerActor extends Actor {
    override def receive: Receive = {
      case "work" =>
        // Long computation
        Thread.sleep(500)
        sender ! WorkResult(42)
      case "workSequence" =>
        val r = new Random()
        for (i <- 1 to 10) {
          Thread.sleep(r.nextInt(50))
          sender() ! WorkResult(1)
        }
    }
  }

}
