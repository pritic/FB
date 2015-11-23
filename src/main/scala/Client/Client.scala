package Client

import Common.Common.Profile
import akka.actor.{Actor, ActorSystem, Props}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import spray.can.Http
import spray.http.HttpMethods._
import spray.http._
import spray.httpx.unmarshalling.FormDataUnmarshallers
import spray.json._

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Created by Priti Changlani on 11/22/15 at 4:41 PM.
  */
object Client {

  def main(args: Array[String]): Unit = {

    val configfactory = ConfigFactory.parseString(
      """
    akka {
      loglevel = "ERROR"
      actor {
        provider = "akka.remote.RemoteActorRefProvider"
      }
      remote {
        enabled-transports = ["akka.remote.netty.tcp"]
        //log-sent-messages = on
        //log-received-messages = on
        netty.tcp {
          hostname = "127.0.0.1"
          port = 0
        }
      }
    }
      """
    )

    val serverIP = args(0)
    val serverPort = args(1)

    implicit var system = ActorSystem("FBClient", ConfigFactory.load
    (configfactory));
    val client = system.actorOf(Props(new ClientActor("1", serverIP,
      serverPort, system)),
      "Client")

    client ! CreateProfile
  }

  class ClientActor(
                     id: String, serverIP: String, serverPort: String,
                     system: ActorSystem)
    extends
    Actor with FormDataUnmarshallers {

    def receive: Receive = {

      case CreateProfile =>

        implicit val timeout = Timeout(10 seconds)

        val jsonTweet = new Profile("id20", "username20", "aboutme20").toJson

        println(jsonTweet)

        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + (serverPort) + "/profile")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), jsonTweet.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)


        println("response: " + response)
        self ! GetProfile("id2")

      case GetProfile(id: String) =>

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + (serverPort) + "/profile/" + id)))

        val response = Await.result(future, timeout.duration)


        println("response: " + response)
    }
  }

}

case class CreateProfile()

case class GetProfile(id: String)
