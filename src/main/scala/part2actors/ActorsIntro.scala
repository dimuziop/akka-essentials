package part2actors

import akka.actor.ActorSystem

/**
 * User: patricio
 * Date: 10/4/21
 * Time: 06:53
 */
object ActorsIntro extends App {

  // part1 - actor system

  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

}
