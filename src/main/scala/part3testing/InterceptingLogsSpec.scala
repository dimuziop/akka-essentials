package part3testing

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.testkit.{EventFilter, ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

/**
 * User: patricio
 * Date: 19/4/21
 * Time: 13:47
 */
class InterceptingLogsSpec extends TestKit(ActorSystem("InterceptingLogSpec", ConfigFactory.load().getConfig("interceptingLogMessages"))) with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll {
  override protected def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import InterceptingLogsSpec._

  val item = "This is just an item"
  val cc = "1234-1234-1234-1234"
  val invalidCc = "0000-0000-0000-0000"

  "A checkout flow" should {

    "correctly log the dispatch of an order" in {
      EventFilter.info(pattern = s"Order [0-9]+ for item $item has been dispatched", occurrences = 1) intercept {
        val checkoutRef = system.actorOf(Props[CheckoutActor])
        checkoutRef ! Checkout(item, cc)
      }
    }

    "freak out if the payment denied" in {
      EventFilter[RuntimeException](occurrences = 1) intercept {
        val checkoutRef = system.actorOf(Props[CheckoutActor])
        checkoutRef ! Checkout(item, invalidCc)
      }
    }
  }


}

object InterceptingLogsSpec {

  case class Checkout(item: String, creditCard: String)

  case class DispatchOrder(item: String)

  case class AuthorizeCard(creditCard: String)

  case object PaymentAccepted

  case object PaymentDenied

  case object OrderConfirmed

  class CheckoutActor extends Actor {

    private val paymentManager = context.actorOf(Props[PaymentManager])
    private val fulfillmentManager = context.actorOf(Props[FulfillmentManager])

    override def receive: Receive = awaitingCheckout


    def awaitingCheckout: Receive = {
      case Checkout(item, card) =>
        paymentManager ! AuthorizeCard(card)
        context.become(pendingPayment(item))
    }

    def pendingPayment(item: String): Receive = {
      case PaymentAccepted =>
        fulfillmentManager ! DispatchOrder(item)
        context.become(pendingFulfillment(item))
      case PaymentDenied => throw new RuntimeException("Unhandled exception")
    }

    def pendingFulfillment(item: String): Receive = {
      case OrderConfirmed => context.become(awaitingCheckout)
    }
  }

  class PaymentManager extends Actor {
    override def receive: Receive = {
      case AuthorizeCard(card) =>
        if (card.startsWith("0")) sender() ! PaymentDenied
        else {
          Thread.sleep(4000)
          sender() ! PaymentAccepted
        }
    }
  }

  class FulfillmentManager extends Actor with ActorLogging {
    var orderId = 43

    override def receive: Receive = {
      case DispatchOrder(item: String) =>
        orderId += 1
        log.info(s"Order ${orderId} for item $item has been dispatched")
        sender() ! OrderConfirmed
    }
  }

}
