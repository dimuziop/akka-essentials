package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

/**
 * User: patricio
 * Date: 14/4/21
 * Time: 08:37
 */
object ChildActorsExercise extends App {

  // DISTRIBUTED WORD COUNTING
  /*

   */

  object WordCounterMaster {
    case class Initialize(nChildren: Int)

    case class WordCountTask(/*TODO*/ text: String)

    case class WordCountReply(text: String, count: Int)
  }

  class WordCounterMaster extends Actor {

    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(children) => context.become(
        withChildren(for (i <- 1 to children) yield context.actorOf(Props[WordCounterWorker], "_" + i),0))
    }

    def withChildren(actorsRef: Seq[ActorRef], taskNumber: Int): Receive = {
      case text: String =>
        println(s"Text: $text")
        actorsRef(taskNumber % actorsRef.length) ! WordCountTask(text)
        context.become(withChildren(actorsRef, taskNumber + 1))

      case WordCountReply(taskName, length) =>
          println(s"The length of $taskName IS $length")

    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._
    override def receive: Receive = {
      case WordCountTask(text) => sender() ! WordCountReply(self.path.name, text.split(" ").length)
    }
  }

  class TestActor extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case "go" =>
        val master = context.actorOf(Props[WordCounterMaster], "master")
        master ! Initialize(3)
        val texts = List("I love Akka", "Scala is super dope", "yes", "me too")
        texts.foreach(text => master ! text)
      case count: Int =>
        println(s"[test actor] I received a reply: $count")
    }
  }

  val system = ActorSystem("childExercise")
  val testActor = system.actorOf(Props[TestActor], "testActor")
  testActor ! "go"

  /*
   create WordCounterMaster
   send Initialize(10) to wordCounterMaster
   send "Akka is awesome" to wordCounterMaster
     wcm will send a WordCountTask("...") to one of its children
       child replies with a WordCountReply(3) to the master
     master replies with 3 to the sender.
   requester -> wcm -> wcw
          r  <- wcm <-
  */
  // round robin logic
  // 1,2,3,4,5 and 7 tasks
  // 1,2,3,4,5,1,2
}
