package part6patterns

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Cancellable, FSM, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, OneInstancePerTest}
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

/**
 * User: patricio
 * Date: 28/4/21
 * Time: 06:33
 */
class FSMSpec extends TestKit(ActorSystem("FSMSpec")) with ImplicitSender with AnyWordSpecLike with BeforeAndAfterAll with OneInstancePerTest {

  import part6patterns.FSMSpec._

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A vending machine" should {
    runTestSuite(Props[VendingMachine])
  }

  "A vending machine FSM" should {
    runTestSuite(Props[VendingMachineFSM])
  }

  def runTestSuite(props: Props): Unit = {
    "error when not initialized" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! RequestProduct("coke")
      expectMsg(VendingError("MachineNotInitialized"))
    }

    "Report a product not available" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map(("dunno" -> 5)), Map("dunno" -> 2))
      vendingMachine ! RequestProduct("coke")
      expectMsg(VendingError("ProductNotAvailable"))
    }

    "Throw a timeout if I don't insert money in" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! Initialize(Map(("coke" -> 5)), Map("coke" -> 1))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 1"))

      within(1.5 seconds) {
        expectMsg(VendingError("RequestTimedOut"))
      }

    }

    "handle the reception of partial money" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map(("coke" -> 5)), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3"))
      vendingMachine ! ReceiveMoney(1)
      expectMsg(Instruction("Please insert 2"))

      within(2 seconds) {
        expectMsg(VendingError("RequestTimedOut"))
        expectMsg(GiveBackChange(1))
      }
    }

    "handle the reception of total money" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map(("coke" -> 5)), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3"))
      vendingMachine ! ReceiveMoney(3)
      expectMsg(Deliver("coke"))

    }

    "handle the reception of total money in partial insertions" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map(("coke" -> 5)), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3"))
      vendingMachine ! ReceiveMoney(1)
      expectMsg(Instruction("Please insert 2"))
      vendingMachine ! ReceiveMoney(2)
      expectMsg(Deliver("coke"))
    }

    "give back change and be able to request money for a new product" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map(("coke" -> 5)), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3"))
      vendingMachine ! ReceiveMoney(4)
      expectMsg(Deliver("coke"))
      expectMsg(GiveBackChange(1))

      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("Please insert 3"))
    }
  }

}

object FSMSpec {

  /*
  Vending machine
   */

  case class Initialize(inventory: Map[String, Int], prices: Map[String, Int])

  case class RequestProduct(product: String)

  case class Instruction(instruction: String) // message to display on this screen

  case class ReceiveMoney(amount: Int)

  case class Deliver(product: String)

  case class GiveBackChange(amount: Int)

  case class VendingError(reason: String)

  case object ReceiveMoneyTimeOut

  class VendingMachine extends Actor with ActorLogging {
    implicit val executionContext: ExecutionContext = context.dispatcher

    override def receive: Receive = idle

    def idle: Receive = {
      case Initialize(inventory, prices) => context.become(operational(inventory, prices))
      case _ => sender() ! VendingError("MachineNotInitialized")
    }

    def operational(inventory: Map[String, Int], prices: Map[String, Int]): Receive = {
      case RequestProduct(product) => inventory.get(product) match {
        case None | Some(0) =>
          sender() ! VendingError("ProductNotAvailable")
        case Some(_) =>
          val price = prices(product)
          sender() ! Instruction(s"Please insert $price")
          context.become(waitForMoney(inventory, prices, product, 0, startReceiveMoneyTimeOut(), sender()))
      }
    }

    def waitForMoney(
                      inventory: Map[String, Int],
                      prices: Map[String, Int],
                      product: String,
                      money: Int,
                      moneyTimoutSchedule: Cancellable,
                      requester: ActorRef): Receive = {
      case ReceiveMoneyTimeOut =>
        requester ! VendingError("RequestTimedOut")
        if (money > 0) requester ! GiveBackChange(money)
        context.become(operational(inventory, prices))
      case ReceiveMoney(amount) =>
        moneyTimoutSchedule.cancel()
        val price = prices(product)
        val partialInsertion = money + amount
        if (partialInsertion >= price) {
          requester ! Deliver(product)
          val change = partialInsertion - price
          if (change > 0) requester ! GiveBackChange(change)
          val newStock = inventory(product) - 1
          val newInventory = inventory + (product -> newStock)
          context.become(operational(newInventory, prices))
        } else {
          val remaining = price - money - amount
          requester ! Instruction(s"Please insert $remaining")
          context.become(waitForMoney(inventory, prices, product, amount, startReceiveMoneyTimeOut(), requester))
        }
    }

    def startReceiveMoneyTimeOut(): Cancellable = context.system.scheduler.scheduleOnce(1 second) {
      self ! ReceiveMoneyTimeOut
    }

  }

  // step 1 - Define the states and data of the actor
  trait VendingState

  case object Idle extends VendingState

  case object Operational extends VendingState

  case object WaitForMoney extends VendingState

  trait VendingData

  case object Uninitialized extends VendingData

  case class Initialized(inventory: Map[String, Int], prices: Map[String, Int]) extends VendingData

  case class WaitForMoneyData(inventory: Map[String, Int],
                              prices: Map[String, Int],
                              product: String,
                              money: Int,
                              requester: ActorRef) extends VendingData

  class VendingMachineFSM extends FSM[VendingState, VendingData] {

    /*
    state, data

    event => state and data can be changed

    state = Idle
    data= Uninitialized
     */

    startWith(Idle, Uninitialized)

    when(Idle) {
      case Event(Initialize(inventory, prices), Uninitialized) =>
        goto(Operational) using Initialized(inventory, prices)
      case _ =>
        sender() ! VendingError("MachineNotInitialized")
        stay()
    }

    when(Operational) {
      case Event(RequestProduct(product), Initialized(inventory, prices)) =>
        inventory.get(product) match {
          case None | Some(0) =>
            sender() ! VendingError("ProductNotAvailable")
            stay()
          case Some(_) =>
            val price = prices(product)
            sender() ! Instruction(s"Please insert $price")
            goto(WaitForMoney) using WaitForMoneyData(inventory, prices, product, 0, sender())
        }

    }

    when(WaitForMoney, stateTimeout = 1 second) {
      case Event(StateTimeout, WaitForMoneyData(inventory, prices, product, money, requester)) =>
        requester ! VendingError("RequestTimedOut")
        if (money > 0) requester ! GiveBackChange(money)
        goto(Operational) using WaitForMoneyData(inventory, prices, product, money, requester)
      case Event(ReceiveMoney(amount), WaitForMoneyData(inventory, prices, product, money, requester)) =>
        val price = prices(product)
        val partialInsertion = money + amount
        if (partialInsertion >= price) {
          requester ! Deliver(product)
          val change = partialInsertion - price
          if (change > 0) requester ! GiveBackChange(change)
          val newStock = inventory(product) - 1
          val newInventory = inventory + (product -> newStock)
          goto(Operational) using Initialized(newInventory, prices)
        } else {
          val remaining = price - money - amount
          requester ! Instruction(s"Please insert $remaining")
          stay() using WaitForMoneyData(inventory, prices, product, amount, requester)
        }
    }

    whenUnhandled {
      case Event(_, _) =>
        sender() ! VendingError("Command No Found")
        stay()
    }

    onTransition {
      case stateA -> stateB => log.info(s"Transitioning from $stateA to $stateB")
    }

    initialize()

  }

}

