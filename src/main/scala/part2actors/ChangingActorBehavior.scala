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
      case Food(VEGETABLE) => context.become(sadReceive, false)//stay sad
      case Food(CHOCOLATE) => context.unbecome()///change to happy
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


}
