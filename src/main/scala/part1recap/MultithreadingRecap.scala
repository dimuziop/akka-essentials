package part1recap

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * User: patricio
 * Date: 9/4/21
 * Time: 13:26
 */
object MultithreadingRecap extends App {

  /*val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel")
  })*/

  val aThread = new Thread(() => println("Running in parallel"))
  aThread.start()
  aThread.join()

  val threadHello = new Thread(() => (1 to 1000).foreach(_ => println("Hello")))
  val threadGoodbye = new Thread(() => (1 to 1000).foreach(_ => println("Goodbye")))

  threadHello.start()
  threadGoodbye.start()

  // different runs produce different results
  class BankAccount(@volatile private var amount: Int) {
    override def toString: String = "" + amount

    def withDraw(amount: Int): Unit = this.amount -= amount

    def safeWithdraw(money: Int): Unit = this.synchronized {
      this.amount -= money
    }
  }


  /*
    BA (10000)
    T1 -> withdraw 1000
    T2 -> withdraw 2000
    T1 -> this.amount = this.amount - .... // PREEMPTED by the OS
    T2 -> this.amount = this.amount - 2000 = 8000
    T1 -> -1000 = 9000
    => result = 9000
    this.amount = this.amount - 1000 is NOT ATOMIC
   */

  // inter-thread communication on the JVM
  // wait - notify mechanism

  // Scala Futures
  import scala.concurrent.ExecutionContext.Implicits.global
  val future = Future {
    // long computation - on a different thread
    42
  }

  // callbacks
  future.onComplete {
    case Success(42) => println("I found the meaning of life")
    case Failure(_) => println("something happened with the meaning of life!")
  }

  val aProcessedFuture = future.map(_ + 1) // Future with 43
  val aFlatFuture = future.flatMap { value =>
    Future(value + 2)
  } // Future with 44

  val filteredFuture = future.filter(_ % 2 == 0) // NoSuchElementException

  // for comprehensions
  val aNonsenseFuture = for {
    meaningOfLife <- future
    filteredMeaning <- filteredFuture
  } yield meaningOfLife + filteredMeaning

  // andThen, recover/recoverWith

  // Promises
}
