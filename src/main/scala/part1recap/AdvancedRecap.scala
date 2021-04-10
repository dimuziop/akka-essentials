package part1recap


import scala.concurrent.Future

/**
 * User: patricio
 * Date: 9/4/21
 * Time: 10:04
 */
object AdvancedRecap extends App {

  val partialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 65
    case 5 => 999
  }

  val pf = (x: Int) => x match {
    case 1 => 42
    case 2 => 65
    case 5 => 999
  }

  println(pf(5))

  val function: (Int => Int) = partialFunction

  val modifiedList = List(1,2,3).map {
    case 1 => 42
    case _ => 0
  }

  val lifted = partialFunction.lift

  println(lifted(2))
  println(lifted(5000))

  //orElse
  val pfChain = partialFunction.orElse[Int, Int] {
    case 60 => 9000
  }

  pfChain(5)
  pfChain(60)
  pfChain(457)

  type ReceiveFunction = PartialFunction[Any, Unit]

  def receive: ReceiveFunction = {
    case 1 => println("Hello")
    case _ => println("confused....")
  }

  implicit val timeout = 3000
  def setTimeout(f: () => Unit)(implicit timeout: Int) = f()

  setTimeout(() => println("Timeout"))

  // implicit conversions
  // 1) implicit defs

  case class Person(name: String) {
    def greet = s"Hi, my name is $name"
  }

  implicit def fromStringToPerson(string: String): Person = Person(string)
  "Peter".greet

  implicit class Dog(name: String) {
    def bark(): Unit = println("Bark!")
  }

  "Lassie".bark() // new Dog("Lassie").bark()

  // organize
  // local scope implicit
  implicit val  inverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  List(1,2,3).sorted

  import scala.concurrent.ExecutionContext.Implicits.global
  val future = Future {
    println("Hello future")
  }

  // companion objects of thew types included in the call

  object Person {
    implicit val personOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }

  List(Person("Bob"), Person("Alice")).sorted





}
