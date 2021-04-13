package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChangingActorBehavior.Mom.MomStart

/**
 * User: patricio
 * Date: 10/4/21
 * Time: 19:02
 */
object ChangingActorBehavior extends App {


  object FussyKid {
    val HAPPY = "happy"
    val SAD = "sad"

    case object KidAccept

    case object KidReject

  }

  class FussyKid extends Actor {

    import FussyKid._
    import Mom._

    var state = HAPPY

    override def receive: Receive = {
      case Food(VEGETABLE) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(_) => if (state == HAPPY) sender() ! KidAccept else sender() ! KidReject
    }
  }

  class StatelessFussyKid extends Actor {

    import FussyKid._
    import Mom._

    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, false) // change to sad
      case Food(CHOCOLATE) =>
      case Ask(_) => sender() ! KidAccept
    }

    def sadReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, false) //stay sad
      case Food(CHOCOLATE) => context.unbecome() ///change to happy
      case Ask(_) => sender() ! KidReject
    }

  }

  object Mom {
    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"

    case class MomStart(kidRef: ActorRef)

    case class Food(food: String)

    case class Ask(message: String)

  }

  class Mom extends Actor {

    import FussyKid._
    import Mom._

    override def receive: Receive = {
      case MomStart(kidRef) =>
        // test our interaction
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Ask("Do you want to play?")
      case KidAccept => println("Yay, my kid is happy")
      case KidReject => println("Yay, my kid is sad, but at least is healthy")
    }
  }

  /**
   * STACK STATE LOOK -> context.become(sadReceive, false)
   */

  val system = ActorSystem("changingActorBehavior")
  val fussyKid = system.actorOf(Props[FussyKid])
  val sFussyKid = system.actorOf(Props[StatelessFussyKid])
  val mom = system.actorOf(Props[Mom])

  //mom ! MomStart(fussyKid)
  mom ! MomStart(sFussyKid)


  /**
   * Exercises
   * 1 - recreate the Counter Actor with context.become and NO MUTABLE STATE
   */

  object Counter {

    case object Increment

    case object Decrement

    case object Print

  }

  class Counter extends Actor {

    import Counter._

    override def receive: Receive = countReceive(0)

    def countReceive(currentCount: Int): Receive = {
      case Increment => context.become(countReceive(currentCount + 1))
      case Decrement => context.become(countReceive(currentCount - 1))
      case Print => println(s"My current count is $currentCount")
    }
  }

  import Counter._

  val counter = system.actorOf(Props[Counter], "theCounter")

  val state: Int = 0

  (1 to 5) foreach (_ => counter ! Increment)
  (1 to 3) foreach (_ => counter ! Decrement)

  counter ! Print


  /**
   * Exercises
   * 2 - simplified voting system
   */

  case class Vote(candidate: String)

  case object VoteStatusRequest

  case class VoteStatusReply(candidate: Option[String])

  class Citizen extends Actor {

    override def receive: Receive = poll(None)

    def poll(name: Option[String]): Receive = {
      case Vote(candidate) => context.become(poll(Some(candidate)))
      case VoteStatusRequest => sender() ! VoteStatusReply(name)
    }
  }

  /*case class AggregateVotes(citizens: Set[ActorRef])

  class VoteAggregator extends Actor {

    override def receive: Receive = pollOpen(Map())

    def pollOpen(value: Map[String, Int]): Receive = {
      case VoteStatusReply(candidate) =>
        if (candidate.isEmpty) context.become(pollOpen(value))
        else {
          if (!value.contains(candidate.get)) {
            println(candidate.get + " new")
            context.become(pollOpen(value + (candidate.get -> 1)))
          }
          else {
            println(candidate.get + " adsd")
            context.become(pollOpen(value + (candidate.get -> (value(candidate.get) + 1))))
          }
        }
      case AggregateVotes(voters: Set[ActorRef]) =>
        if (voters.nonEmpty) {
          voters.head ! VoteStatusRequest
          self ! AggregateVotes(voters.tail)
        }
        println(value)
    }
  }*/

  case class AggregateVotes(citizens: Set[ActorRef])
  class VoteAggregator extends Actor {
    override def receive: Receive = awaitingCommand

    def awaitingCommand: Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest)
        context.become(awaitingStatuses(citizens, Map()))
    }

    def awaitingStatuses(stillWaiting: Set[ActorRef], currentStats: Map[String, Int]): Receive = {
      case VoteStatusReply(None) =>
        // a citizen hasn't voted yet
        sender() ! VoteStatusRequest // this might end up in an infinite loop
      case VoteStatusReply(Some(candidate)) =>
        val newStillWaiting = stillWaiting - sender()
        val currentVotesOfCandidate = currentStats.getOrElse(candidate, 0)
        val newStats = currentStats + (candidate -> (currentVotesOfCandidate + 1))
        if (newStillWaiting.isEmpty) {
          println(s"[aggregator] poll stats: $newStats")
        } else {
          // still need to process some statuses
          context.become(awaitingStatuses(newStillWaiting, newStats))
        }
    }
  }

  val lisa = system.actorOf(Props[Citizen], "Lisa")
  val bart = system.actorOf(Props[Citizen], "Bart")
  val bort = system.actorOf(Props[Citizen], "Bort")
  val abel = system.actorOf(Props[Citizen], "Abel")
  val moe = system.actorOf(Props[Citizen], "Moe")

  lisa ! Vote("Martin")
  bart ! Vote("Jonas")
  bort ! Vote("Roland")
  abel ! Vote("Roland")
  moe ! Vote("Roland")

  val voteAggregator = system.actorOf(Props[VoteAggregator], "voteAggregator")
  voteAggregator ! AggregateVotes(Set(lisa, bart, bort, abel, moe))

  /**
   * Print the status of the votes
   * Martin -> 1
   * Jonas -> 1
   * Roland -> 2
   */


}
