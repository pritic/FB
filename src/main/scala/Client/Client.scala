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

      //      client ! GetProfile("user" + i)
      //      client ! GetTimeline("user" + i)
    }

    system.actorSelection("/user/1") ! MakeFriend("user2")
    Thread.sleep(1000)

    system.actorSelection("/user/1") ! PostMessageToFriend("user2",
      "Hi I am a message post", "friends")

    Thread.sleep(1000)

    system.actorSelection("/user/1") ! PostMessageToFriend("user3",
      "Hi I am a message post", "friends")

    Thread.sleep(1000)

    system.actorSelection("/user/1") ! PostMessageToFriend("user4",
      "Hi I am a message post", "public")

    Thread.sleep(1000)

    system.actorSelection("/user/4") ! PostMessageToFriend("user2",
      "Hi I am a private post dont dare read me", "private")

    //    system.actorSelection("/user/1") ! GetMyPosts
    //    Thread.sleep(1000)
    //    system.actorSelection("/user/2") ! GetMyPosts
    Thread.sleep(1000)



    //    system.actorSelection("/user/1") ! GetFriendList("user1")
    //    Thread.sleep(1000)
    //    system.actorSelection("/user/2") ! GetFriendList("user1")
    //    Thread.sleep(1000)
    //    system.actorSelection("/user/2") ! GetFriendList("user2")

    system.actorSelection("/user/1") ! GetTimeline("user2")

    system.actorSelection("/user/1") ! PostPicture("album1","public")
    system.actorSelection("/user/1") ! PostPicture("album2","public")
    system.actorSelection("/user/1") ! PostPicture("album3","friends")
    system.actorSelection("/user/1") ! PostPicture("album1","friends")
    system.actorSelection("/user/1") ! PostPicture("album1","private")
    system.actorSelection("/user/1") ! PostPicture("album5", "friends")
    Thread.sleep(1000)
    system.actorSelection("/user/1") ! GetAlbum("user2","album1")
    Thread.sleep(1000)
    system.actorSelection("/user/1") ! GetPictures("user2")


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

    //    friendList += "i am a friend"

    def receive: Receive = {

      case CreateUser =>

        implicit val timeout = Timeout(10 seconds)

        val userJSON = new User(id, username, about, postList, friendList).toJson

        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/user")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), userJSON.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)


        println("responseCreateProfile: " + response)

      case GetProfile(id: String) =>

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + (serverPort) + "/profile/" + id)))

        val response = Await.result(future, timeout.duration)


        println("responseProfile: " + response)

      case GetTimeline(id1: String) =>

        implicit val timeout = Timeout(10 seconds)

        //        val timelineRequestJSON = new TimelineRequest(id, id1).toJson

        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/timeline?sender=" +
          id + "&requested=" + id1)))

        val response = Await.result(future, timeout.duration)


        println("response: GetTimeline: " + response)

      case PostMessageToFriend(friendID: String, content: String,
      privacy: String) =>


        val postJSON = new Post(System.currentTimeMillis().toString, id, friendID, privacy, content)
          .toJson

        implicit val timeout = Timeout(10 seconds)

        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/post")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), postJSON.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)


        println("response: PostMessageToFriend: " + response)

      case GetMyPosts =>

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/post/" + id)))

        val response = Await.result(future, timeout.duration)

        println("response: GetMyPosts: " + response)


      case MakeFriend(id1: String) =>


        implicit val timeout = Timeout(10 seconds)

        val friendRequestJSON = new FriendRequest(id, id1).toJson

        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/makefriend")).withEntity
        (HttpEntity(ContentType(MediaTypes.`application/json`), friendRequestJSON.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)


        println("response: MakeFriend: " + id + " is making friends" + response)

      case GetFriendList(id1: String) =>
        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/getfriends/" + id1)))

        val response = Await.result(future, timeout.duration)

        println("response: GetFriendList: " + id + " is getting friends" +
          response)


      case PostPicture(albumID: String
      ,privacy: String) =>

        val pixelImagebase64String = "R0lGODlhAQABAIAAAP///wAAACwAAAAAAQABAAACAkQBADs="
        val imageJSON = new Picture(System.currentTimeMillis().toString, id, albumID, privacy, pixelImagebase64String)
          .toJson

        implicit val timeout = Timeout(10 seconds)

        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/picture")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), imageJSON.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)

        println("response: PostPicture: " + response)

      case GetPictures(id1: String) =>

        implicit val timeout = Timeout(10 seconds)

        //        val timelineRequestJSON = new TimelineRequest(id, id1).toJson

        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/picture?sender=" +
          id + "&requested=" + id1)))

        val response = Await.result(future, timeout.duration)


        println("response: GetPictures: " + response)

      case GetAlbum(requestorID: String,albumID: String) =>

        implicit val timeout = Timeout(10 seconds)

        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/album?albumowner="+id+"&requestorid=" +
          requestorID + "&requestedalbum=" + albumID)))

        val response = Await.result(future, timeout.duration)


        println("response: GetAlbum: " + response)


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

case class MakeFriend(id: String)

case class GetFriendList(id: String)

case class PostPicture(albumID: String,privacy: String)

case class GetAlbum(requestorID: String,albumID: String)

case class GetPictures(id1: String)