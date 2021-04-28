package part6patterns

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike
import part6patterns.AskSpec.AuthManager.{AUTH_FAILURE_NOT_FOUND, AUTH_FAILURE_PASSWORD_INCORRECT, AUTH_FAILURE_SYSTEM}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

/**
 * User: patricio
 * Date: 28/4/21
 * Time: 04:23
 */
class AskSpec extends TestKit(ActorSystem("AskSpec"))
  with ImplicitSender with AnyWordSpecLike with BeforeAndAfterAll {

  import AskSpec._

  "An authenticator" should {
    authenticatorTestSuite(Props[AuthManager])
  }

  "A piped authenticator" should {
    authenticatorTestSuite(Props[PipedAuthManager])
  }

  def authenticatorTestSuite(props: Props) = {
    "fail to authenticate a non-registered user" in {
      val authManager = system.actorOf(props)
      authManager ! Authenticate("me", "rtjvm")
      expectMsg(AuthFailure(AUTH_FAILURE_NOT_FOUND))
    }

    "fail to authenticate if invalid password" in {
      val authManager = system.actorOf(props)
      authManager ! RegisterUser("me", "rtjvm")
      authManager ! Authenticate("me", "sdfsdf")
      expectMsg(AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT))
    }

    "successfully authenticated" in {
      val authManager = system.actorOf(props)
      authManager ! RegisterUser("me", "rtjvm")
      authManager ! Authenticate("me", "rtjvm")
      expectMsg(AuthSuccess)
    }
  }


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

  object AuthManager {
    val AUTH_FAILURE_NOT_FOUND = "username not found"
    val AUTH_FAILURE_PASSWORD_INCORRECT = "password incorrect"
    val AUTH_FAILURE_SYSTEM = "system error"
  }

  class AuthManager extends Actor with ActorLogging {

    // step 2 - logistics

    implicit val timeout: Timeout = Timeout(1 second)
    implicit val executionContext: ExecutionContext = context.dispatcher
    protected val authDb = context.actorOf(Props[KVActor])


    override def receive: Receive = {
      case RegisterUser(username, password) => authDb ! Write(username, password)
      case Authenticate(username, password) => handleAuthentication(username, password)
    }

    def handleAuthentication(username: String, password: String): Any = {
      val originalSender = sender()
      // step 3 - ask the actor
      val future = authDb ? Read(username)
      // step 4 - handle the future for e.g. with onComplete
      future.onComplete {
        // step 5 most important
        // NEVER CALL METHODS ON THE ACTOR INSTANCE OR ACCESS MUTABLE STATE IN CONCOMPLETE
        // avoid closing over the actor instance or mutable state
        case Success(None) => originalSender ! AuthFailure(AUTH_FAILURE_NOT_FOUND)
        case Success(Some(dbPassword)) =>
          if (dbPassword == password) originalSender ! AuthSuccess
          else originalSender ! AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT)
        case Failure(_) => originalSender ! AuthFailure(AUTH_FAILURE_SYSTEM)
      }
    }
  }

  class PipedAuthManager extends AuthManager {
    override def handleAuthentication(username: String, password: String): Any = {
      // step 3 - ask the actor
      val future = authDb ? Read(username)
      // step 4 - process the future until you get the responses you will send back
      val passwordFuture = future.mapTo[Option[String]]
      val responseFuture = passwordFuture.map {
        case None => AuthFailure(AUTH_FAILURE_NOT_FOUND)
        case Some(dbPassword) =>
          if (dbPassword == password) AuthSuccess
          else AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT)
      }// Future[Any]
      // step 5 - pipe the resulting future to the actor you want to send the result to
      /*
        When the future completes, send the response to the actor ref in the arg list.
       */
      responseFuture.pipeTo(sender())
    }
  }

}
