package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

/**
 * User: patricio
 * Date: 14/4/21
 * Time: 17:18
 */
class BasicSpec extends TestKit(ActorSystem("BasicSpec"))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll {

  // setup
  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import part3testing.BasicSpec._

  "A simple actor" should {
    "send the same message" in {
      val echoActor = system.actorOf(Props[SimpleActor])
      val message = "this is a test message"
      echoActor ! message

      expectMsg(message) // akka.teat.single-expect-default
    }
  }

  "A blackHole actor" should {
    "send black hole some message" in {
      val echoActor = system.actorOf(Props[BlackHole])
      val message = "this is a test message"
      echoActor ! message

      expectNoMessage(1 second)
      //testActor - implicit sender
    }
  }

  // message assertions
  "A lab test actor" should {
    val labTestActor = system.actorOf(Props[LabTestActor])
    "turn a string into uppercase" in {

      labTestActor ! "I love aKka"
      expectMsg("I LOVE AKKA")

    }

    "typical assertions" in {

      labTestActor ! "I love aKka"
      val reply = expectMsgType[String]
      assert(reply == "I LOVE AKKA")

    }

    "reply to a greeting" in {
      labTestActor ! "greeting"
      expectMsgAnyOf("hi", "hello")
    }

    "reply with Scala AND Akka" in {
      labTestActor ! "favoriteTech"
      expectMsgAllOf("Scala", "Akka")
    }

    "reply with cool tech in a different way" in {
      labTestActor ! "favoriteTech"
      val messages = receiveN(2) // Seq[Any]
      // free to do more complicated assertions
    }

    "reply with cool tech in a fancy way" in {
      labTestActor ! "favoriteTech"
      expectMsgPF() {
        case "Scala" => // only care that PF is defined
        case "Akka" =>
      }
    }

  }

}

object BasicSpec {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message => sender() ! message
    }
  }

  class BlackHole extends Actor {
    override def receive: Receive = Actor.emptyBehavior
  }

  class LabTestActor extends Actor {
    var random = new Random()
    override def receive: Receive = {
      case "greeting" => if (random.nextBoolean()) sender() ! "hi" else sender() ! "hello"
      case "favoriteTech" =>
        sender() ! "Scala"
        sender() ! "Akka"
      case message: String => sender() ! message.toUpperCase
    }
  }
}
