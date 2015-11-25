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
      client ! PostMessageToFriend("user" + i,"public","MessageContent")
    }


    system.actorSelection("/user/1") ! PostMessageToFriend("user2",
      "friends", "Hi I am a message post")
    system.actorSelection("/user/1") ! PostMessageToFriend("user3",
      "friends", "Hi I am a message post")
    system.actorSelection("/user/1") ! PostMessageToFriend("user4",
      "friends", "Hi I am a message post")

    system.actorSelection("/user/1") ! GetMyPosts
    Thread.sleep(100)
    system.actorSelection("/user/2") ! GetMyPosts

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

    var postList: Map[String, Post] = new scala.collection.immutable
    .HashMap[String, Post]
    var friendList: Set[String] = new scala.collection.immutable
    .HashSet[String]

    //    postList.add("i am a post")
    //    friendList.add("i am a friend")

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

      case PostMessageToFriend(friendID: String, content: String,
      privacy: String) =>
        //        val post = new Post(System.currentTimeMillis().toString, id, friendID,
        //          privacy, content)

        //        postList = postList + ("pp1" -> post)

        val postJSON = new Post(System.currentTimeMillis().toString, id, friendID, privacy, content)
          .toJson

        implicit val timeout = Timeout(10 seconds)

        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/post")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), postJSON.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)


        println("response: PostMessageToFriend: " + response)
      //        println(postList.get("pp1"))

      case GetMyPosts =>
        //        println(postList)

        implicit val timeout = Timeout(10 seconds)
        println("i am in getmypost, id: " + id)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/post/" + id)))

        val response = Await.result(future, timeout.duration)

        println("response: GetMyPosts: " + response)

    }
  }

}

case class CreateUser()

case class GetProfile(id: String)

case class GetTimeline(id: String)

case class PostMessageToFriend(
                                friendID: String, content: String,
                                privacy: String)

case class GetMyPosts()
