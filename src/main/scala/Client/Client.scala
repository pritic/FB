package Client

import java.util

import Common.Common._
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

import scala.collection.immutable._
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
    val userIDList: util.HashSet[String] = new util.HashSet[String]()

    implicit var system = ActorSystem("FBClient", ConfigFactory.load
    (configfactory))

    for (i <- 1 until 5) {
      userIDList.add("user" + i)

      val client = system.actorOf(Props(new FBUser("user" + i, "name" + i,
        "about" + i, serverIP, serverPort, system)), i.toString)

      client ! CreateUser
      client ! GetProfile("user" + i)
      client ! GetTimeline("user" + i)
    }
  }

  class FBUser(
                userID: String,
                name: String,
                aboutMe: String,
                serverIP: String,
                serverPort: String,
                system: ActorSystem
              ) extends Actor with FormDataUnmarshallers {

    val id: String = userID
    var username: String = name
    var about: String = aboutMe
    var postList: Set[String] = new scala.collection.immutable.HashSet[String]
    var friendList: Set[String] = new scala.collection.immutable
    .HashSet[String]

    //    postList.add("i am a post")
    //    friendList.add("i am a friend")

    postList += "i am a post"
    friendList += "i am a friend"

    def receive: Receive = {

      case CreateUser =>

        implicit val timeout = Timeout(10 seconds)

        val userJSON = new User(id, username, about, postList, friendList).toJson

        //        println(userJSON)

        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/user")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), userJSON.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)


        println("responseCreateProfile: " + response)
      //        self ! GetProfile(id)

      case GetProfile(id: String) =>

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + (serverPort) + "/profile/" + id)))

        val response = Await.result(future, timeout.duration)


        println("responseProfile: " + response)

      case GetTimeline(id: String) =>

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + (serverPort) + "/timeline/" + id)))

        val response = Await.result(future, timeout.duration)


        println("responseTimeline: " + response)
    }
  }

}

case class CreateUser()

case class GetProfile(id: String)

case class GetTimeline(id: String)
