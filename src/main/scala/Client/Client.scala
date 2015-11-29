package Client

import java.util

import Common.Common._
import akka.actor.{PoisonPill, Actor, ActorSystem, Props}
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
  privacyList.put(1, "public")
  privacyList.put(2, "private")

  def main(args: Array[String]): Unit = {

    val configFactory = ConfigFactory.parseString(
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
    (configFactory))

    for (i <- 1 to numOfUsers) {
      userIDList.add("user" + i)

      val client = system.actorOf(Props(new FBUser("user" + i, "name" + i,
        "about" + i, serverIP, serverPort, system)), i.toString)

      client ! CreateUser

      system.scheduler.schedule(100 millis, 5000 millis, client, Schedule
      ("user" + Random.nextInt(userIDList.size())))
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
    var postList: Map[String, Post] = new scala.collection.immutable
    .HashMap[String, Post]
    var friendList: Set[String] = new scala.collection.immutable
    .HashSet[String]

    def receive: Receive = {

      case Schedule(id1: String) =>

        self ! GetProfile(id1)
        self ! MakeFriend(id1)
        self ! GetFriendList(id1)
        self ! PostMessage(id1, "This is a post from " + id + " to " +
          id1, privacyList.get(Random.nextInt(3)))
        self ! GetMyPosts
        self ! GetTimeline(id1)
        self ! PostPicture("album1", privacyList.get(Random.nextInt(2)))
        self ! PostPicture("album2", privacyList.get(Random.nextInt(2)))
        self ! GetPictures(id1)
        self ! GetAlbum(id1, "album1")
        self ! GetAlbum(id1, "album2")

        Thread.sleep(100)
        self ! PoisonPill



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

      case PostPicture(albumID: String, privacy: String) =>

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

case class Schedule(id: String)

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