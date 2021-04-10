package part1recap

import scala.concurrent.Future

/**
 * User: patricio
 * Date: 10/4/21
 * Time: 05:56
 */
object ThreadModelLimitations extends App {

  /*

   */

  /**
   * DN #1 00P encapsulation is only valid un the SINGLE THREAD MODEL.
   */
  /*class BankAccount(private var amount: Int) {
    override def toString: String = "" + amount

    def withDraw(amount: Int): Unit = this.amount -= amount

    def deposit(amount: Int): Unit = this.amount += amount

    def getAmount: Int = amount

  }*/

  class BankAccount(private var amount: Int) {
    override def toString: String = "" + amount

    def withDraw(amount: Int): Unit = this.synchronized {
      this.amount -= amount
    }

    def deposit(amount: Int): Unit = this.synchronized {
      this.amount += amount
    }

    def getAmount: Int = amount

  }

  val account = new BankAccount(2000)
  /*for (_ <- 1 to 1000) {
    new Thread(() => account.withDraw(1)).start()
  }

  for (_ <- 1 to 1000) {
    new Thread(() => account.deposit(1)).start()
  }*/

  println(account.getAmount)

  // OOP is broken in a multithreading env
  // synchronization Locks to the rescue

  // deadlocks, livelocks

  /**
   * DR #2: delegating something to a thread is a PAIN
   */

  // you have a running and you want to pass a runnable to that thread

  var task: Runnable = null

  val runningThread: Thread = new Thread(() => {
    while (true) {
      while (task == null) {
        runningThread.synchronized{
          println("[background] waiting for a task...")
          runningThread.wait()
        }
      }
      task.synchronized{
        println("[background] I have a task...")
        task.run()
        task = null
      }
    }
  })

  def delegateToBackgroundThread(r: Runnable): Unit = {
    if( task == null) task = r
    runningThread.synchronized {
      runningThread.notify()
    }
  }

  runningThread.start()
  Thread.sleep(500)
  delegateToBackgroundThread(() => println(42))

  Thread.sleep(1000)
  delegateToBackgroundThread(() => println("This should run in the backgorund"))

  /**
   * DR #3: tracing and dealing with errors in a multithreading env is a PITN
   */

  // 1M numbers in between 10 threads

  import scala.concurrent.ExecutionContext.Implicits.global

  val futures = (1 to 10) map (i => 100000 * i until 100000 * (i + 1)) map ( range => Future {
    if (range.contains(546735)) throw new RuntimeException("invalid number")
    range.sum
  })

  val futureSum = Future.reduceLeft(futures)(_ + _)
  futureSum.onComplete(println)


}
