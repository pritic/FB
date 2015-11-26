package Server

import java.util

import akka.actor._
import akka.io.IO
import akka.util.Timeout
import akka.pattern.ask
import com.typesafe.config.ConfigFactory
import spray.can.Http
import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport._
import spray.routing._

import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.language.postfixOps
import spray.httpx.SprayJsonSupport._

/**
  * Created by Priti Changlani on 11/22/15 at 4:45 PM.
  */
object Server {

  def main(args: Array[String]): Unit = {

    val configfactory = ConfigFactory.parseString(
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

    val config = ConfigFactory.load(configfactory)
    val host = config.getString("http.host")
    val port = config.getInt("http.port")

    implicit val system = ActorSystem("FB", ConfigFactory.load(configfactory));

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

  //  var quizzes = Vector[Quiz]()

  var users = Vector[User]()

  var posts: Map[String, Post] = new scala.collection.immutable
  .HashMap[String, Post]


  var allUserPostsMap: Map[String, List[Post]] = new scala.collection.immutable.HashMap[String, List[Post]]

  var allUserFriendMap: Map[String, List[String]] = new scala.collection.immutable.HashMap[String, List[String]]

  def routes: Route =

    pathPrefix("user") {
      path(Segment) { id =>
        get { requestContext =>
          val responder = createResponder(requestContext)
          getUser(id).map(responder ! _)
            .getOrElse(responder ! ProfileNotFound)
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
      pathPrefix("timeline") {
        path(Segment) { id =>
          get { requestContext =>
            val responder = createResponder(requestContext)
            getTimeline(id).map(responder ! _)
              .getOrElse(responder ! ProfileNotFound)
          }
        }
      } ~
      pathPrefix("profile") {
        path(Segment) { id =>
          get { requestContext =>
            val responder = createResponder(requestContext)
            getProfile(id).map(responder ! _)
              .getOrElse(responder ! ProfileNotFound)
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
                  .getOrElse(responder ! ProfileNotFound)
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
              .getOrElse(responder ! ProfileNotFound)
          }
        }
      }

  private def createResponder(requestContext: RequestContext) = {

    context.actorOf(Props(new Responder(requestContext)))
  }

  private def createProfile(user: User): Boolean = {

    val doesNotExist = !users.exists(_.id == user.id)
    if (doesNotExist) {
      users = users :+ user
      print(users)
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

  private def getPosts(id: String): Option[List[Post]] = {

    println("i am in server:getPosts; id: " + id)
    allUserPostsMap.get(id).map(toPosts)
  }

  private def getUser(id: String): Option[User] = {

    getUserID(id).map(toUser)
  }

  private def getProfile(id: String): Option[Profile] = {

    getUserID(id).map(toProfile)
  }

  private def getTimeline(id: String): Option[TimeLine] = {

    getUserID(id).map(toTimeline)
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

  private def getFriends(id:String): Option[List[String]] = {
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

    case friends:String =>
      requestContext.complete(StatusCodes.OK, friends)
      killYourself

    case ProfileNotFound =>
      requestContext.complete(StatusCodes.NotFound)
      killYourself

    case FriendsMade =>
      requestContext.complete(StatusCodes.OK)
      killYourself

  }

  private def killYourself = self ! PoisonPill

}
