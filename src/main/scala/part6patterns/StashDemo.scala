package part6patterns

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Stash}

/**
 * User: patricio
 * Date: 27/4/21
 * Time: 06:01
 */
object StashDemo extends App {

  /*
  ResourceActor --- Access to a file or whatever
   - open => it can receive read/write requests to the resource
   - otherwise it will postpone all read/write requests until the state is open

   ResourceActor is closed
    - Open => switch the state
     - Read, write will be postponed

   ResourceActor is open
    - Read, write are handled
    - Close switch to closed state

    [Open, Read, Read, Write]
    - switch tot he open state
    - read the data
    - read the data again
    - write the data

    [Read, Open, Write]
     - stash Read
      Stash: [Read]
      - open => switch to open state
      Mailbox [Read, Write]
      -read and write are handled

   */

  case object Open
  case object Close
  case object Read
  case class Write(data: String)


  //step1 - mix-in stash trait
  class ResourceActor extends Actor with ActorLogging with Stash {

    private var innerData: String = ""

    override def receive: Receive = closed

    def closed: Receive = {
      case Open =>
        log.info("opening resource")
        // step 3 - unstashAll when you switch the message handler
        unstashAll()
        context.become(open)
      case message =>
        log.info(s"Stashing $message because I can't handle it in the closed state")
        // step 2 - stash away what you can handle
        stash()
    }

    def open: Receive = {
      case Read =>
        // do some actual computation
        log.info("Inner data read")
      case Write(data) =>
        log.info(s"I'm am writing: $data")
      case Close =>
        log.info(s"Closing resource")
        unstashAll()
        context.become(closed)
      case message =>
        log.info(s"Stashing $message because I can't handle it in the open state")
        // step 2 - stash away what you can handle
        stash()
    }

  }

  val system = ActorSystem("stashDemo")

  val resourceActor = system.actorOf(Props[ResourceActor])

  resourceActor ! Read
  resourceActor ! Open
  resourceActor ! Open
  resourceActor ! Write("I love stash")
  resourceActor ! Close
  resourceActor ! Read




}
