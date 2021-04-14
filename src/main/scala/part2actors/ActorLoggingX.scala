package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging

/**
 * User: patricio
 * Date: 14/4/21
 * Time: 13:46
 */
object ActorLoggingX extends App {

  // #1 - explicit logging

  class SimpleActorWithExplicitLogger extends Actor {

    val logger = Logging(context.system, this)

    override def receive: Receive = {
      case message => logger.info(message.toString)
    }
  }

  val system = ActorSystem("LogingSystem")
  val actor = system.actorOf(Props[SimpleActorWithExplicitLogger], "someName")

  actor ! "Logging a message"

  // #2 - actor loging

  class ActorWithLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a, b) => log.info("Two things: {} and {}", a, b)
      case message => log.info(message.toString)
    }
  }

  val simpleActor = system.actorOf(Props[ActorWithLogging])
  simpleActor ! "Logging from the trait extended log "
  simpleActor ! (42, 56)

}
