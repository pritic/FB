package Common

/**
  * Created by Priti Changlani on 11/22/15 at 4:52 PM.
  */
object Common {

  case class Profile(id: String, username: String, about: String)

  case object ProfileNotFound

  import spray.json._

  //  case class Profile (id: String, username: String, about: String)
  //
    case object ProfileCreated

    case object ProfileAlreadyExists

  case class Quiz(id: String, question: String, correctAnswer: String)

  case object QuizCreated

  case object QuizAlreadyExists

  case object QuizDeleted

  case class Question(id: String, question: String)

  case object QuestionNotFound

  case class Answer(answer: String)

  case object CorrectAnswer

  case object WrongAnswer

  /* json (un)marshalling */

  object Quiz extends DefaultJsonProtocol {

    implicit val format = jsonFormat3(Quiz.apply)
  }

  object Question extends DefaultJsonProtocol {

    implicit val format = jsonFormat2(Question.apply)
  }

  object Answer extends DefaultJsonProtocol {

    implicit val format = jsonFormat1(Answer.apply)
  }

  object Profile extends DefaultJsonProtocol {

    implicit val format = jsonFormat3(Profile.apply)
  }

  /* implicit conversions */

  implicit def toQuestion(quiz: Quiz): Question = Question(id = quiz.id, question = quiz.question)

  implicit def toAnswer(quiz: Quiz): Answer = Answer(answer = quiz.correctAnswer)

  implicit def toProfile(profile: Profile): Profile = Profile(id = profile
    .id, username = profile.username, about = profile.about)
}
