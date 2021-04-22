package part4faulttolerance

import akka.actor.SupervisorStrategy.{Escalate, Restart, Resume, Stop}
import akka.actor.{Actor, ActorRef, ActorSystem, AllForOneStrategy, OneForOneStrategy, Props, SupervisorStrategy, Terminated}
import akka.testkit.{EventFilter, ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

/**
 * User: patricio
 * Date: 22/4/21
 * Time: 03:47
 */
class SupervisionSpec extends TestKit(ActorSystem("SupervisionSpec"))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll
{

  import SupervisionSpec._

  "A supervisor" should {
    "resume its child in case of a minor fault" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "I love akka"
      child ! Report
      expectMsg(3)

      child ! "This is a long sentence made in akka to make a child throw an exception"
      child ! Report
      expectMsg(3)

    }

    "restart its child in case of an empty sentence" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "I love akka"
      child ! Report
      expectMsg(3)

      child ! ""
      child ! Report
      expectMsg(0)

    }

    "supervisor should terminate its child in case of a major error" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]
      watch(child)
      child ! "akka is nice"

      val terminatedMessage = expectMsgType[Terminated]
      assert(terminatedMessage.actor == child)
    }

    "supervisor should scalate an error when doesnt know what to do" in {
      val supervisor = system.actorOf(Props[Supervisor], "supervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      watch(child)
      child ! 43
      val terminatedMessage = expectMsgType[Terminated]
      assert(terminatedMessage.actor == child)
    }

  }

  "a kinder supervisor" should {
    "not kill all children in case it's restarted or escalates failures" in {
      val supervisor = system.actorOf(Props[NoDeathOnRestart], "kinder_supervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "Akka is chido"
      child ! Report
      expectMsg(3)

      child ! 45
      child ! Report
      expectMsg(0)
    }
  }

  "An all-for-one supervisor" should {
    "apply the all-for-one strategy" in {
      val supervisor = system.actorOf(Props[AllForOneSupervisor], "allForOneSupervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      supervisor ! Props[FussyWordCounter]
      val secondChild = expectMsgType[ActorRef]

      secondChild ! "Testing supervision"
      secondChild ! Report
      expectMsg(2)

      EventFilter[NullPointerException]() intercept {
        child ! ""
      }

      Thread.sleep(500)

      secondChild ! Report
      expectMsg(0)
    }
  }




  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)
}

object SupervisionSpec {

  class Supervisor extends Actor {

    override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
      case _: NullPointerException => Restart
      case _: IllegalArgumentException => Stop
      case _: RuntimeException => Resume
      case _: Exception => Escalate
    }

    override def receive: Receive = {
      case props: Props =>
        val childRef = context.actorOf(props)
        sender() ! childRef
    }
  }

  class NoDeathOnRestart extends Supervisor {
    override def preRestart(reason: Throwable, message: Option[Any]): Unit = ()
  }

  class AllForOneSupervisor extends Supervisor {
    override val supervisorStrategy: AllForOneStrategy = AllForOneStrategy() {
      case _: NullPointerException => Restart
      case _: IllegalArgumentException => Stop
      case _: RuntimeException => Resume
      case _: Exception => Escalate
    }
  }

  case object Report
  class FussyWordCounter extends Actor {

    var words = 0

    override def receive: Receive = {
      case Report => sender() ! words
      case "" => throw new NullPointerException("sentence is empty")
      case sentence: String =>
        if (sentence.length > 20) throw new RuntimeException("sentence is too big")
        else if (!Character.isUpperCase(sentence(0))) throw new IllegalArgumentException("sentence must start with uppercase")
        else words += sentence.split(" ").length
      case _ => throw new Exception("can only receive strings")
    }
  }


}
