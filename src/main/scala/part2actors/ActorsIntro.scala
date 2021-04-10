package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

/**
 * User: patricio
 * Date: 10/4/21
 * Time: 06:53
 */
object ActorsIntro extends App {

  // part1 - actor system

  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  // part 2 - create actors
  // word count actor

  class WordCountActor extends Actor {
    // internal data
    var totalWords = 0

    // behavior
    override def receive: Receive = {
      case message: String =>
        println(s"[word counter] I have receive $message")
        totalWords += message.split(" ").length
      case meg => println(s"I cant understand ${meg.toString}")
    }
  }

  // part3 - instantiate our actor
  val wordCounter: ActorRef = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter: ActorRef = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")

  //part4 - communicate
  wordCounter ! "I'm learning Akka and it's a pretty damn cool" // "tell"
  anotherWordCounter ! "A different message"
  //asynchronous

  object Person {
    def props(name: String): Props = Props(new Person(name))
  }

  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi my name is $name")
      case _ =>
    }
  }

  val person = actorSystem.actorOf(Person.props("Bob"))

  person ! "hi"




}
