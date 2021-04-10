package part2actors

import akka.actor.Status.Failure
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ActorCapabilities.Person.LiveTheLife

/**
 * User: patricio
 * Date: 10/4/21
 * Time: 07:52
 */
object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    // context.self === THIS
    override def receive: Receive = {
      case "Hi!" => sender() ! "Hello there!" // reply to a message
      case message: String => println(s"[Simple ${self.path}] I have received a $message")
      case number: Int => println(s"[Simple actor] I have received a NUMBER: $number")
      case SpecialMessage(contents) => println(s"[Simple actor] I have received a SPECIAL MESSAGE: $contents")
      case SendMessageToYourself(content) =>
        self ! content
      case SayHiTo(ref) => ref ! "Hi"
      case WirelessPhoneMessage(content, ref) => ref forward (content + "s")
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"
  //for (ln <- io.Source.stdin.getLines) simpleActor ! ln

  // 1 messages can be of any type
  // a) messages must be IMMUTABLE
  // b) messages must be SERIALIZABLE
  // in practice use case classes and case objects

  simpleActor ! 42

  case class SpecialMessage(contents: String)
  simpleActor ! SpecialMessage("Some content")

  // 2 - actors have information about their contest and about themselves

  case class SendMessageToYourself(context: String)
  simpleActor ! SendMessageToYourself("Send a message to yourself")

  // 3 - actors can REPLY to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

  // 4 - dead letters
  alice ! "Hi!" // reply to "me"

  // 5 - forwarding messages
  // D -> A -> B
  // forwarding = sending a message with the ORIGINAL sender

  case class WirelessPhoneMessage(content: String, ref: ActorRef)
  alice ! WirelessPhoneMessage("Hi", bob) // noSender.

  /**
   * Exercises
   *
   * 1. a Counter actor
   *   - Increment
   *   - Decrement
   *   - Print
   *
   * 2. a Bank account as an actor
   *   receives
   *   - Deposit an amount
   *   - Withdraw an amount
   *   - Statement
   *   replies with
   *   - Success
   *   - Failure
   *
   *   interact with some other kind of actor
   */

  object Counter {
    case class Increase(amount: Int)
    case class Decrease(amount: Int)
    case class Print()
  }

  class Counter extends Actor {
    import Counter._
    var actualCount: Int = 0
    override def receive: Receive = {
      case Increase(amount) => actualCount += amount
      case Decrease(amount) => actualCount -= amount
      case Print => println(actualCount)
    }
  }
  import Counter._



  val counter = system.actorOf(Props[Counter], "counter")

  counter ! Increase(1)
  counter ! Increase(1)
  counter ! Increase(1)
  counter !Print
  counter ! Decrease(1)
  counter ! Decrease(1)
  counter !Print

  object BankAccount {
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object Statement

    case class TransactionSuccess(message: String)
    case class TransactionFailure(message: String)
  }

  class BankAccount(initialCapital: Int = 0) extends Actor {
    import BankAccount._
    var founds: Int = initialCapital
    override def toString: String = "" + founds

    def withDraw(amount: Int): Unit = {
      if (amount < 0) sender() ! TransactionFailure("Invalid withdraw amount")
      else if (amount > this.founds) sender() ! TransactionFailure("You shall not do this")
      else {
        this.founds -= amount
        sender() ! TransactionSuccess(s"You've withdrew: $amount")
      }
      Failure
    }

    def deposit(amount: Int): Unit = {
      if (amount < 0) sender() ! TransactionFailure("Invalid deposit amount")
      else {
        this.founds += amount
        sender() ! TransactionSuccess(s"Deposit increased by $amount")
      }
    }

    def getAmount: Int = founds

    override def receive: Receive = {
      case Deposit(amount) => this.deposit(amount)
      case Withdraw(amount) => this.withDraw(amount)
      case Statement => sender() ! s"Your balance is $founds"
    }
  }
  import BankAccount._

  object Person {
    case class LiveTheLife(account: ActorRef)
  }

  class Person extends Actor {
    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(90000)
        account ! Withdraw(500)
        account ! Statement
      case message => println(message.toString)
    }
  }



  val myAccount = system.actorOf(Props(new BankAccount(5)), "myAccount")
  val person = system.actorOf(Props[Person], "myself")

  person ! LiveTheLife(myAccount)

}
