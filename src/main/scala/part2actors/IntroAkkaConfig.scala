package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

/**
 * User: patricio
 * Date: 14/4/21
 * Time: 14:04
 */
object IntroAkkaConfig extends App {

  class ActorWithLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a, b) => log.info("Two things: {} and {}", a, b)
      case message => log.info(message.toString)
    }
  }

  /**
   * 1- Inline configuraiton
   */

  val configString =
    """
      | akka {
      |   loglevel = "ERROR"
      | }
    """.stripMargin

  val config = ConfigFactory.parseString(configString)
  val system = ActorSystem("configurationDemo", ConfigFactory.load(config))
  val actor = system.actorOf(Props[ActorWithLogging])

  actor ! "A message to remember"

  /**
   * 2 - configuration file
   */

  val defaultConfigFileSystem = ActorSystem("configurationDemo")
  val defaultConfigActor = defaultConfigFileSystem.actorOf(Props[ActorWithLogging])

  defaultConfigActor ! "Remember me"

  /**
   * 3- separate configuration in the same file
   */
  val specialConfig = ConfigFactory.load().getConfig("mySpecialConfig")
  val specialConfigSystem = ActorSystem("specialConfigDemo", specialConfig)
  val specialConfigActor = specialConfigSystem.actorOf(Props[ActorWithLogging])

  specialConfigActor ! "As well"

  /**
   * 4- separate config in another file
   */
  val separateConfig = ConfigFactory.load("secretFolder/secret.conf")
  println(separateConfig.getString("akka.loglevel"))

  /**
   * 5 - different file formats
   */

  val jsonConfig = ConfigFactory.load("json/jsonConfig.json")
  println(jsonConfig.getString("aJsonProperty"))

  val propsConfig = ConfigFactory.load("props/propsConfiguration.properties")
  println(propsConfig.getString("my.simpleProperty"))



}
