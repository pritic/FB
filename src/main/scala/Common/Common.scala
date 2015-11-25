package Common


import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.httpx.SprayJsonSupport._

/**
  * Created by Priti Changlani on 11/22/15 at 4:52 PM.
  */



object Common {



  case class User(
                   id: String,
                   username: String,
                   about: String,
                   postList: scala.collection.immutable.Map[String, Post],
                   friendList: scala.collection.immutable.Set[String]
                 )

  case class Profile(username: String, about: String, friendList: scala.collection.immutable.Set[String])

  case class TimeLine(username: String, postList: scala.collection.immutable
  .Map[String, Post])


  case class Post(
                   postID: String, from: String, to: String, privacy: String,
                   content: String)

  case object ProfileNotFound

  case object ProfileCreated

  case object PostCreated

  case object ProfileAlreadyExists

  //  case class Quiz(id: String, question: String, correctAnswer: String)
  //
  //  case object QuizCreated
  //
  //  case object QuizAlreadyExists
  //
  //  case object QuizDeleted
  //
  //  case class Question(id: String, question: String)
  //
  //  case object QuestionNotFound
  //
  //  case class Answer(answer: String)
  //
  //  case object CorrectAnswer
  //
  //  case object WrongAnswer

  /* json (un)marshalling */

  //  object Quiz extends DefaultJsonProtocol {
  //
  //    implicit val format = jsonFormat3(Quiz.apply)
  //  }
  //
  //  object Question extends DefaultJsonProtocol {
  //
  //    implicit val format = jsonFormat2(Question.apply)
  //  }
  //
  //  object Answer extends DefaultJsonProtocol {
  //
  //    implicit val format = jsonFormat1(Answer.apply)
  //  }

  object Post extends DefaultJsonProtocol {

    implicit val format = jsonFormat5(Post.apply)
  }

  object User extends DefaultJsonProtocol {

    implicit val format = jsonFormat5(User.apply)
  }


  object Profile extends DefaultJsonProtocol {

    implicit val format = jsonFormat3(Profile.apply)
  }

  object TimeLine extends DefaultJsonProtocol {

    implicit val format = jsonFormat2(TimeLine.apply)
  }

  /* implicit conversions */

  //  implicit def toQuestion(quiz: Quiz): Question = Question(id = quiz.id, question = quiz.question)
  //
  //  implicit def toAnswer(quiz: Quiz): Answer = Answer(answer = quiz.correctAnswer)

  implicit def toUser(user: User): User = User(
    id = user.id,
    username = user.username,
    about = user.about,
    postList = user.postList,
    friendList = user.friendList)

  implicit def toProfile(user: User): Profile = Profile(username =
    user.username, about = user.about, friendList = user.friendList)

  implicit def toTimeline(user: User): TimeLine = TimeLine(username =
    user.username, postList = user.postList)

  implicit def toPost(post: Post): Post = Post(postID = post.postID, from =
    post.from, to = post.to, privacy = post.privacy, content = post.content)
}
