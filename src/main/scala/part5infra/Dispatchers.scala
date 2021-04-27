package part5infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}

import scala.concurrent.{ExecutionContext, Future}

/**
 * User: patricio
 * Date: 27/4/21
 * Time: 03:27
 */
object Dispatchers extends App {

  class Counter extends Actor with ActorLogging {

    var count = 0

    override def receive: Receive = {
      case message =>
        count += 1
        log.info(s"[$count] - $message")
    }
  }

  val system = ActorSystem("DispatchersDemo"/*, ConfigFactory.load().getConfig("dispatchersDemo")*/)

  // Method no 1 - pprogramatic, in code
  val actors = for (i <- 1 to 10) yield system.actorOf(Props[Counter].withDispatcher("my-dispatcher"), s"s_counter${i}")

  /*var r = new Random()
  for (i <- 1 to 1000) {
    actors(r.nextInt(10)) ! i
  }*/


  // Method no 2 - from config
  val rtjvmActor = system.actorOf(Props[Counter], "rtjvm")

  /**
   * Dispatcher implement the ExecutionContext trait
   */
  class DBActor extends Actor with ActorLogging {
    // dedicated dispatcher for blocking actors (solution1)
    implicit val executionContext: ExecutionContext = context.system.dispatchers.lookup("my-dispatcher")
    // solution 2 -using a router
    override def receive: Receive = {
      case message => Future {
        // wait on a resource
        Thread.sleep(5000)
        log.info(s"success: $message")
      }
    }
  }

  val dbActor = system.actorOf(Props[DBActor])
  dbActor ! "the meaning of life is 42"

  val nonBlockingActor = system.actorOf(Props[Counter])
  for (i <- 1 to 1000) {
    val message = s"important message $i"
    dbActor ! message
    nonBlockingActor ! message
  }


}
