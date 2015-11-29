package Server

import akka.actor._
import akka.io.IO
import akka.util.Timeout
import akka.pattern.ask
import com.typesafe.config.ConfigFactory
import spray.can.Http
import spray.http.{MediaTypes, StatusCodes}
import spray.httpx.SprayJsonSupport._
import spray.routing._

import scala.collection.immutable.{Map}
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Created by Priti Changlani on 11/22/15 at 4:45 PM.
  */
object Server {

  def main(args: Array[String]): Unit = {

    val configFactory = ConfigFactory.parseString(
      """
    akka {
      loglevel = INFO
      stdout-loglevel = INFO
      loggers = ["akka.event.slf4j.Slf4jLogger"]
      default-dispatcher {
        fork-join-executor {
          parallelism-min = 8
        }
      }
      test {
        timefactor = 1
      }
    }
    spray {
      can {
        server {
          server-header = "Facebook API"
        }
      }
    }
    http {
      host = "127.0.0.1"
      host = ${?HOST}
      port = 5000
      port = ${?PORT}
    }
      """
    )

    val config = ConfigFactory.load(configFactory)
    val host = config.getString("http.host")
    val port = config.getInt("http.port")

    implicit val system = ActorSystem("FB", ConfigFactory.load(configFactory));

    val master = system.actorOf(Props(new RestInterface()), "httpInterface")

    implicit val executionContext = system.dispatcher
    implicit val timeout = Timeout(10 seconds)

    IO(Http).ask(Http.Bind(listener = master, interface = host, port = port))
      .mapTo[Http.Event]
      .map {
        case Http.Bound(address) =>
          println(s"REST interface bound to $address")
        case Http.CommandFailed(cmd) =>
          println("REST interface could not bind to " +
            s"$host:$port, ${cmd.failureMessage}")
          system.shutdown()
      }

  }

}

class RestInterface extends HttpServiceActor with RestApi {

  def receive = runRoute(routes)
}

trait RestApi extends HttpService with ActorLogging {
  actor: Actor =>

  import Common.Common._

  implicit val timeout = Timeout(50 seconds)

  var users = Vector[User]()

  var posts: Map[String, Post] = new scala.collection.immutable
  .HashMap[String, Post]

  var allUserPostsMap: Map[String, List[Post]] = new scala.collection.immutable.HashMap[String, List[Post]]

  var allUserFriendMap: Map[String, List[String]] = new scala.collection.immutable.HashMap[String, List[String]]

  var allUserPicturesMap: Map[String, List[Picture]] = new scala.collection.immutable.HashMap[String, List[Picture]]

  def routes: Route =

    pathPrefix("user") {
      path(Segment) { id =>
        get { requestContext =>
          val responder = createResponder(requestContext)
          getUser(id).map(responder ! _)
            .getOrElse(responder ! NotFound)
        }
      } ~
        pathEnd {
          post {
            entity(as[User]) { profile => requestContext =>
              val responder = createResponder(requestContext)
              createProfile(profile) match {
                case true => responder ! ProfileCreated
                case _ => responder ! ProfileAlreadyExists
              }
            }
          }
        }
    } ~
      (path("timeline") & get) {
        parameters("sender", "requested") { (from, to) => requestContext =>
          val responder = createResponder(requestContext)
          responder ! getTimeline(from, to)
        }
      } ~
      pathPrefix("profile") {
        path(Segment) { id =>
          get { requestContext =>
            val responder = createResponder(requestContext)
            getProfile(id).map(responder ! _)
              .getOrElse(responder ! NotFound)
          }
        }
      } ~
      pathPrefix("post") {
        pathEnd {
          post {
            entity(as[Post]) { post => requestContext =>
              val responder = createResponder(requestContext)
              createPost(post)
              responder ! PostCreated
            }
          }
        } ~
          path(Segment) { id =>
            get {
              requestContext =>
                val responder = createResponder(requestContext)
                getPosts(id).map(responder ! _)
                  .getOrElse(responder ! NotFound)
            }
          }
      } ~
      pathPrefix("makefriend") {
        pathEnd {
          post {
            entity(as[FriendRequest]) { friendRequest => requestContext =>
              val responder = createResponder(requestContext)
              makeFriend(friendRequest)
              responder ! FriendsMade
            }
          }
        }
      } ~
      pathPrefix("getfriends") {
        path(Segment) { id =>
          get { requestContext =>
            val responder = createResponder(requestContext)
            getFriends(id).map(responder ! _.toString())
              .getOrElse(responder ! NotFound)
          }
        }
      } ~
      pathPrefix("picture") {
        pathEnd {
          post {
            entity(as[Picture]) { picture => requestContext =>
              val responder = createResponder(requestContext)
              createPicture(picture)
              responder ! ImageCreated
            }
          }
        }
      } ~
      (path("picture") & get) {
        parameters("sender", "requested") { (from, to) => requestContext =>
          val responder = createResponder(requestContext)
          responder ! getPictures(from, to)
        }
      } ~
      (path("album") & get) {
        parameters("albumowner", "requestorid", "requestedalbum") { (owner, requestedby, albumID) => requestContext =>
          val responder = createResponder(requestContext)
          responder ! getAlbum(owner, requestedby, albumID)
        }
      }

  private def createResponder(requestContext: RequestContext) = {

    context.actorOf(Props(new Responder(requestContext)))
  }

  private def createProfile(user: User): Boolean = {

    val doesNotExist = !users.exists(_.id == user.id)
    if (doesNotExist) {
      users = users :+ user
      //      print(users)
    }
    doesNotExist
  }

  private def createPost(post: Post): Unit = {

    allUserPostsMap.get(post.from) match {
      case Some(x) =>
        val y = post :: x
        allUserPostsMap = allUserPostsMap + (post.from -> y)
      case None =>
        val y = List(post)
        allUserPostsMap = allUserPostsMap + (post.from -> y)
    }

    allUserPostsMap.get(post.to) match {
      case Some(x) =>
        val y = post :: x
        allUserPostsMap = allUserPostsMap + (post.to -> y)
      case None =>
        val y = List(post)
        allUserPostsMap = allUserPostsMap + (post.to -> y)
    }
  }

  private def createPicture(picture: Picture): Unit = {

    allUserPicturesMap.get(picture.from) match {
      case Some(x) =>
        val y = picture :: x
        allUserPicturesMap = allUserPicturesMap + (picture.from -> y)
      case None =>
        val y = List(picture)
        allUserPicturesMap = allUserPicturesMap + (picture.from -> y)
    }
  }

  private def getPosts(id: String): Option[List[Post]] = {
    allUserPostsMap.get(id).map(toPosts)
  }

  private def getUser(id: String): Option[User] = {

    getUserID(id).map(toUser)
  }

  private def getProfile(id: String): Option[Profile] = {

    getUserID(id).map(toProfile)
  }

  private def getUsersName(id: String): Option[String] = {

    getUserID(id).map(toUsersName)
  }

  private def getTimeline(from: String, to: String): List[Post]
  = {

    println("========================================")
    println(from + " is requesting " + to + "'s timeline")
    var requestedPostList: List[Post] = List[Post]()
    var isFriend: Boolean = false
    println(allUserPostsMap.get(to))
    println(allUserFriendMap.get(from))

    if (from.equalsIgnoreCase(to)) {
      println(from + " is requesting its own timeline")
      allUserPostsMap.get(to) match {
        case Some(x) =>
          x.foreach(
            y =>
              requestedPostList = requestedPostList :+ y
          )
          println(requestedPostList)
        case None =>
          println("getTimeline: " + from + " has no posts")
      }
    }
    else {
      allUserFriendMap.get(from) match {
        case Some(a) =>
          if (a.contains(to))
            isFriend = true
          else
            println("getTimeline: not a friend")
        case None =>
          println("getTimeline: " + from + " does not have any friends")
      }

      if (isFriend) {
        allUserPostsMap.get(to) match {
          case Some(x) =>
            x.foreach(
              y => if (
                (y.privacy.equalsIgnoreCase("public") ||
                  y.privacy.equalsIgnoreCase("friends"))
                  && y.to.equalsIgnoreCase(from)
                  || y.from.equalsIgnoreCase(from))
                requestedPostList = requestedPostList :+ y
            )
            println(requestedPostList)
          case None =>
            println("getTimeline: " + to + " has no posts")
        }
      }
      else {
        allUserPostsMap.get(to) match {
          case Some(x) =>
            //          println("x: " + x)
            x.foreach(
              y => if (
                y.privacy.equalsIgnoreCase("public")
                  || y.to.equalsIgnoreCase(from)
                  || y.from.equalsIgnoreCase(from))
                requestedPostList = requestedPostList :+ y
            )
            println(requestedPostList)
          case None =>
            println("getTimeline: " + to + " has no posts")
        }
      }
    }
    println("========================================")

    requestedPostList
  }

  private def getPictures(from: String, to: String): List[Picture]
  = {

    println("in server getPictures: 1")
    var requestedPictureList: List[Picture] = List[Picture]()
    var isFriend: Boolean = false


    allUserFriendMap.get(from) match {
      case Some(a) =>
        if (a.contains(to))
          isFriend = true
        else
          println("in server:getPictures: not a friend")
      case None =>
        println("in server:getPictures: allUserFriendMap.get(from) did not " +
          "return anything")
    }

    if (isFriend) {
      allUserPicturesMap.get(from) match {
        case Some(x) =>
          x.foreach(
            y => if (
              y.privacy.equalsIgnoreCase("public") ||
                y.privacy.equalsIgnoreCase("friends")
            )
              requestedPictureList = requestedPictureList :+ y
          )
        case None =>
          println("in server:getPictures: No pictures whatsoever")
      }
    }
    else {
      allUserPicturesMap.get(from) match {
        case Some(x) =>
          x.foreach(
            y => if (
              y.privacy.equalsIgnoreCase("public")
            )
              requestedPictureList = requestedPictureList :+ y
          )
        case None =>
          println("in server:getPictures: No pictures whatsoever")
      }
    }
    println("requestedPictureList: " + requestedPictureList)

    requestedPictureList
  }

  private def getAlbum(owner: String, requestedby: String, albumID: String): List[Picture]
  = {

    println("in server getAlbum: 1")
    var requestedAlbumList: List[Picture] = List[Picture]()
    var isFriend: Boolean = false

    allUserFriendMap.get(owner) match {
      case Some(a) =>
        if (a.contains(requestedby))
          isFriend = true
        else
          println("in server:getAlbum: not a friend")
      case None =>
        println("in server:getAlbum: allUserFriendMap.get(owner) did not " +
          "return anything")
    }

    if (isFriend) {
      allUserPicturesMap.get(owner) match {
        case Some(x) =>
          x.foreach(
            y => if (
              (y.privacy.equalsIgnoreCase("public") ||
                y.privacy.equalsIgnoreCase("friends"))
                && y.albumID.equalsIgnoreCase(albumID)
            )
              requestedAlbumList = requestedAlbumList :+ y
          )
        case None =>
          println("in server:getTimeline: No albums whatsoever")
      }
    }
    else {
      allUserPicturesMap.get(owner) match {
        case Some(x) =>
          x.foreach(
            y => if (
              y.privacy.equalsIgnoreCase("public")
                && y.albumID.equalsIgnoreCase(albumID)
            )
              requestedAlbumList = requestedAlbumList :+ y
          )
        case None =>
          println("in server:getTimeline: No albums whatsoever")
      }
    }

    requestedAlbumList
  }

  private def getUserID(id: String): Option[User] = {

    users.find(_.id == id)
  }

  private def makeFriend(friendRequest: FriendRequest): Unit = {

    allUserFriendMap.get(friendRequest.from) match {
      case Some(x) =>
        val y = friendRequest.to :: x
        allUserFriendMap = allUserFriendMap + (friendRequest.from -> y)
      case None =>
        val y = List(friendRequest.to)
        allUserFriendMap = allUserFriendMap + (friendRequest.from -> y)
    }

    allUserFriendMap.get(friendRequest.to) match {
      case Some(x) =>
        val y = friendRequest.from :: x
        allUserFriendMap = allUserFriendMap + (friendRequest.to -> y)
      case None =>
        val y = List(friendRequest.from)
        allUserFriendMap = allUserFriendMap + (friendRequest.to -> y)
    }
  }

  private def getFriends(id: String): Option[List[String]] = {

    println("i am in server:getFriends; id: " + id)
    allUserFriendMap.get(id).map(toFriends)
  }
}

class Responder(requestContext: RequestContext) extends Actor with ActorLogging {

  import Common.Common._

  def receive = {

    case ProfileCreated =>
      requestContext.complete(StatusCodes.Created)
      killYourself

    case PostCreated =>
      requestContext.complete(StatusCodes.Created)
      killYourself

    case ImageCreated =>
      requestContext.complete(StatusCodes.Created)
      killYourself

    case ProfileAlreadyExists =>
      requestContext.complete(StatusCodes.Conflict)
      killYourself

    case user: User =>
      requestContext.complete(StatusCodes.OK, user)
      killYourself

    case profile: Profile =>
      requestContext.complete(StatusCodes.OK, profile)
      killYourself

    case timeline: TimeLine =>
      requestContext.complete(StatusCodes.OK, timeline)
      killYourself

    case posts: List[Post] =>
      requestContext.complete(StatusCodes.OK, posts)
      killYourself

    case AlbumResponse1 =>
      println("in server case album/picture")
      requestContext.complete(StatusCodes.OK)
      killYourself

    case friends: String =>
      println("in server: case friends")
      requestContext.complete(StatusCodes.OK, friends)
      killYourself

    case NotFound =>
      requestContext.complete(StatusCodes.NotFound)
      killYourself

    case FriendsMade =>
      requestContext.complete(StatusCodes.OK)
      killYourself

  }

  private def killYourself = self ! PoisonPill

}