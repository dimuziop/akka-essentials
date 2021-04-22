package part4faulttolerance

import akka.actor.{Actor, ActorLogging, ActorSystem, PoisonPill, Props}

/**
 * User: patricio
 * Date: 21/4/21
 * Time: 13:16
 */
object ActorLifecycle extends App {

  object StartChild

  class LifeCycleActor extends Actor with ActorLogging {


    override def preStart(): Unit = log.info("I am starting")
    override def postStop(): Unit = log.info("I have stopped")

    override def receive: Receive = {
      case StartChild =>
        log.info("Parent initialized")
        context.actorOf(Props[LifeCycleActor], "child")
    }
  }

  val system = ActorSystem("LifecycleDemo")
  val parent = system.actorOf(Props[LifeCycleActor], "parent")

  parent ! StartChild
  parent ! PoisonPill

  /**
   * Restart
   */

  object Fail
  object Check
  object FailChild
  object CheckChild

  class Parent extends Actor {

    private val child = context.actorOf(Props[Child], "supervisedChild")

    override def receive: Receive = {
      case FailChild => child ! Fail
      case CheckChild => child ! Check
    }
  }

  class Child extends Actor with ActorLogging {

    override def preStart(): Unit = log.info("Supervised child started")
    override def postStop(): Unit = log.info("Supervised child stopped")

    override def preRestart(reason: Throwable, message: Option[Any]): Unit = log.info(s"Supervised child restarting due to  ${reason.getMessage}")

    override def postRestart(reason: Throwable): Unit = log.info(s"Supervised child restarted due to  ${reason.getMessage}")

    override def receive: Receive = {
      case Fail => log.warning("child will fail now")
        throw new RuntimeException("I failed")
      case Check => log.warning("Alive and kicking")
    }
  }

  val supervisor = system.actorOf(Props[Parent], "v")
  supervisor ! FailChild
  supervisor ! CheckChild

  // supervision strategy

}
