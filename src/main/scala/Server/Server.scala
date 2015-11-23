package Server

import akka.actor._
import akka.io.IO
import akka.util.Timeout
import akka.pattern.ask
import com.typesafe.config.ConfigFactory
import spray.can.Http
import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport._
import spray.routing._

import scala.concurrent.duration._
import scala.language.postfixOps

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

  var quizzes = Vector[Quiz]()

  var users = Vector[User]()

  print(users)

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
      }

  private def createResponder(requestContext: RequestContext) = {

    context.actorOf(Props(new Responder(requestContext)))
  }

  private def createQuiz(quiz: Quiz): Boolean = {

    val doesNotExist = !quizzes.exists(_.id == quiz.id)
    if (doesNotExist) {
      quizzes = quizzes :+ quiz
      print(quizzes)
    }
    doesNotExist
  }

  private def createProfile(profile: User): Boolean = {

    val doesNotExist = !users.exists(_.id == profile.id)
    if (doesNotExist) {
      users = users :+ profile
      print(users)
    }
    doesNotExist
  }

  private def deleteQuiz(id: String): Unit = {

    quizzes = quizzes.filterNot(_.id == id)
  }

  private def getRandomQuestion: Option[Question] = {

    !quizzes.isEmpty match {
      case true =>
        import scala.util.Random
        val idx = (new Random).nextInt(quizzes.size)
        Some(quizzes(idx))
      case _ => None
    }
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

  private def getQuestion(id: String): Option[Question] = {

    getQuiz(id).map(toQuestion)
  }

  private def getQuiz(id: String): Option[Quiz] = {

    quizzes.find(_.id == id)
  }

  private def isAnswerCorrect(id: String, proposedAnswer: Answer): Boolean = {

    getQuiz(id).exists(_.correctAnswer == proposedAnswer.answer)
  }
}

class Responder(requestContext: RequestContext) extends Actor with ActorLogging {

  import Common.Common._

  def receive = {

    case QuizCreated =>
      requestContext.complete(StatusCodes.Created)
      killYourself

    case ProfileCreated =>
      requestContext.complete(StatusCodes.Created)
      killYourself

    case QuizDeleted =>
      requestContext.complete(StatusCodes.OK)
      killYourself

    case QuizAlreadyExists =>
      requestContext.complete(StatusCodes.Conflict)
      killYourself

    case ProfileAlreadyExists =>
      requestContext.complete(StatusCodes.Conflict)
      killYourself

    case question: Question =>
      requestContext.complete(StatusCodes.OK, question)
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

    case QuestionNotFound =>
      requestContext.complete(StatusCodes.NotFound)
      killYourself

    case ProfileNotFound =>
      requestContext.complete(StatusCodes.NotFound)
      killYourself

    case CorrectAnswer =>
      requestContext.complete(StatusCodes.OK)
      killYourself

    case WrongAnswer =>
      requestContext.complete(StatusCodes.NotFound)
      killYourself
  }

  private def killYourself = self ! PoisonPill

}
