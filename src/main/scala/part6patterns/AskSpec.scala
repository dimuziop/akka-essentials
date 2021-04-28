package part6patterns

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success

/**
 * User: patricio
 * Date: 28/4/21
 * Time: 04:23
 */
class AskSpec extends TestKit(ActorSystem("AskSpec"))
  with ImplicitSender with AnyWordSpecLike with BeforeAndAfterAll {


  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

}

object AskSpec {

  //this code will be somewhere else in your app

  case class Read(key: String)
  case class Write(key: String, value: String)

  class KVActor extends Actor with ActorLogging {
    override def receive: Receive = online(Map())

    def online(kv: Map[String, String]): Receive = {
      case Read(key) =>
        log.info(s"Trying to read the value at the key $key")
        sender() ! kv.get(key) // Option[String]
      case Write(key, value) =>
        log.info(s"writting the value $value for the key $key")
        context.become(online(kv + (key -> value)))
    }

  }

  // user authenticator actos

  case class RegisterUser(username: String, password: String)
  case class Authenticate(username: String, password: String)
  case class AuthFailure(message: String)
  case object AuthSuccess

  /*
  Not worth complicated behavior

  class AuthManager extends Actor with ActorLogging {

    private val authDb = context.actorOf(Props[KVActor])

    override def receive: Receive = {
      case RegisterUser(username, password) => authDb ! Write(username, password)
      case Authenticate(username, password) =>
        authDb ! Read(username)
        context.become(waitingForPassword(username, sender()))
    }

    def waitingForPassword(str: String, ref: ActorRef): Unit = {
      case password: Option[String] => //do passweord checks
    }
  }*/

  class AuthManager extends Actor with ActorLogging {

    // step 2 - logistics

    implicit val timeout: Timeout = Timeout(1 second)
    implicit val executionContext: ExecutionContext = context.dispatcher
    private val authDb = context.actorOf(Props[KVActor])


    override def receive: Receive = {
      case RegisterUser(username, password) => authDb ! Write(username, password)
      case Authenticate(username, password) =>
        // step 2 - ask the actor
        val future = authDb ? Read(username)
        // step 4 - handle the future for e.g. with onComplete
        future.onComplete {
              // step 5 most important
              // NEVER CALL METHODS ON THE ACTOR INSTANCE OR ACCESS MUTABLE STATE IN CONCOMPLETE
          case Success(None) => sender() ! AuthFailure("password not found")
          case Success(Some(dbPassword)) =>
            if (dbPassword == password) sender() ! AuthSuccess
            else sender() !AuthFailure("password incorrect")
        }

    }
  }

}
