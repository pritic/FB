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
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by Priti Changlani on 11/22/15 at 4:41 PM.
  */
object Client {

  val userIDList: util.HashSet[String] = new util.HashSet[String]()
  val privacyList: util.HashMap[Int, String] = new util.HashMap[Int, String]()
  privacyList.put(0, "friends")
  privacyList.put(1, "private")
  privacyList.put(2, "public")

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
    val numOfUsers = args(2).toInt

    implicit var system = ActorSystem("FBClient", ConfigFactory.load
    (configfactory))

    for (i <- 1 to numOfUsers) {
      userIDList.add("user" + i)

      val client = system.actorOf(Props(new FBUser("user" + i, "name" + i,
        "about" + i, serverIP, serverPort, system)), i.toString)

      client ! CreateUser
      system.scheduler.schedule(5000 millis, 5000 millis, client, Schedule
      ("user" + Random.nextInt(userIDList.size())))
    }

//    system.actorSelection("/user/1") ! MakeFriend("user2")
//    Thread.sleep(1000)
//
//    //    Thread.sleep(1000)
//
//    system.actorSelection("/user/1") ! PostMessage("user2",
//      "Hi I am a message post", "friends")
//
//    Thread.sleep(1000)
//
//    system.actorSelection("/user/1") ! PostMessage("user3",
//      "Hi I am a message post", "friends")
//
//    Thread.sleep(1000)
//
//    system.actorSelection("/user/1") ! PostMessage("user4",
//      "Hi I am a message post", "public")
//
//    Thread.sleep(1000)
//
//    system.actorSelection("/user/4") ! PostMessage("user2",
//      "Hi I am a private post dont dare read me", "private")
//
//    system.actorSelection("/user/1") ! GetMyPosts
//    Thread.sleep(1000)
//    system.actorSelection("/user/2") ! GetMyPosts
//    Thread.sleep(1000)



    //    system.actorSelection("/user/1") ! GetFriendList("user1")
    //    Thread.sleep(1000)
    //    system.actorSelection("/user/2") ! GetFriendList("user1")
    //    Thread.sleep(1000)
    //    system.actorSelection("/user/2") ! GetFriendList("user2")

//    system.actorSelection("/user/1") ! GetTimeline("user2")
//
//    system.actorSelection("/user/2") ! PostPicture("album1", "public")
//    system.actorSelection("/user/1") ! PostPicture("album2", "public")
//    system.actorSelection("/user/2") ! PostPicture("album3", "friends")
//    system.actorSelection("/user/2") ! PostPicture("album1", "friends")
//    system.actorSelection("/user/1") ! PostPicture("album1", "public")
//    system.actorSelection("/user/1") ! PostPicture("album5", "friends")
//    Thread.sleep(1000)
//    system.actorSelection("/user/1") ! GetAlbum("user2", "album1")
//    Thread.sleep(1000)
//    system.actorSelection("/user/1") ! GetPictures("user2")
//    Thread.sleep(1000)
//    system.actorSelection("/user/3") ! GetPictures("user2")

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

      case Schedule(id1: String) =>

        val m = Random.nextInt(userIDList.size())


//        self ! GetProfile("user" + m)
//        self ! GetFriendList("user" + m)
        self ! GetTimeline(id1)
        self ! PostMessage(id1, "This is a post from " + id + " to " +
          id1, privacyList.get(Random.nextInt(3)))



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

      case PostMessage(friendID: String, content: String,
      privacy: String) =>


        val postJSON = new Post(System.currentTimeMillis().toString, id, friendID, privacy, content)
          .toJson

        implicit val timeout = Timeout(10 seconds)

        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/post")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), postJSON.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)


        println("response: PostMessage: " + response)

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
      , privacy: String) =>

        val is = this.getClass.getClassLoader().getResourceAsStream("picture.jpg");
        val stream = Stream.continually(is.read).takeWhile(_ != -1).map(_.toByte)
        val bytes = stream.toArray
        val pictureBase64String = new sun.misc.BASE64Encoder().encode(bytes)

        //val pixelImagebase64String = "R0lGODlhAQABAIAAAP///wAAACwAAAAAAQABAAACAkQBADs="
        val imageJSON = new Picture(System.currentTimeMillis().toString, id, albumID, privacy, pictureBase64String)
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
          id1 + "&requested=" + id)))

        val response = Await.result(future, timeout.duration)

        println("response: GetPictures: " + response)

      case GetAlbum(requestorID: String, albumID: String) =>

        implicit val timeout = Timeout(10 seconds)

        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/album?albumowner=" + requestorID + "&requestorid=" +
          id + "&requestedalbum=" + albumID)))

        val response = Await.result(future, timeout.duration)


        println("response: GetAlbum: " + response)

    }
  }

}

case class Schedule(id:String)
case class CreateUser()

case class GetProfile(id: String)

case class GetTimeline(id: String)

case class PostMessage(
                                friendID: String, content: String,
                                privacy: String)

case class GetMyPosts()

case class MakeFriend(id: String)

case class GetFriendList(id: String)

case class PostPicture(albumID: String, privacy: String)

case class GetAlbum(requestorID: String, albumID: String)

case class GetPictures(id1: String)