package Common

import java.security.PublicKey

import spray.json._

/**
  * Created by Priti Changlani on 11/22/15 at 4:52 PM.
  */
object Common {

  case class User(
                   id: String,
                   username: String,
                   about: String,
                   publicKey: String,
                   postList: scala.collection.immutable.Map[String, Post],
                   friendList: scala.collection.immutable.Set[String]
                 )

  case class Profile(username: String, about: String)

  case class TimeLine(
                       username: String, postList: scala.collection.immutable
  .Map[String, Post])

  case class FriendRequest(from: String, to: String)

  case class TimelineResponse(
                               requestor: String, responseList: scala
  .collection.immutable.List[Post])

  case class Post(
                   date: String, from: String, to: String, privacy: String,
                   content: String)

  case class Friend(username: String, friendList: scala.collection.immutable.Set[String])

  case class Picture(date: String, from: String, albumID: String, privacy: String, pictureContent: String)

  case class Album(requestor: String, responseList: scala.collection.immutable.List[Picture])

  case class PictureList(responseList: scala.collection.immutable.List[Picture])

  case object NotFound

  case object ProfileCreated

  case object PostCreated

  case object ImageCreated

  case object ProfileAlreadyExists

  case object FriendsMade

  case object TimelineResponse1

  case object AlbumResponse1

  /* json (un)marshalling */

  object Friend extends DefaultJsonProtocol {

    implicit val format = jsonFormat2(Friend.apply)
  }

  object Picture extends DefaultJsonProtocol {

    implicit val format = jsonFormat5(Picture.apply)
  }

  object Album extends DefaultJsonProtocol {

    implicit val format = jsonFormat2(Album.apply)
  }

  object FriendRequest extends DefaultJsonProtocol {

    implicit val format = jsonFormat2(FriendRequest.apply)
  }

  object Post extends DefaultJsonProtocol {

    implicit val format = jsonFormat5(Post.apply)
  }

  object TimelineResponse extends DefaultJsonProtocol {

    implicit val format = jsonFormat2(TimelineResponse.apply)
  }

  object User extends DefaultJsonProtocol {

    implicit val format = jsonFormat6(User.apply)
  }

  object Profile extends DefaultJsonProtocol {

    implicit val format = jsonFormat2(Profile.apply)
  }

  object TimeLine extends DefaultJsonProtocol {

    implicit val format = jsonFormat2(TimeLine.apply)
  }

  /* implicit conversions */

  implicit def toUser(user: User): User = User(
    id = user.id,
    username = user.username,
    about = user.about,
    publicKey = user.publicKey,
    postList = user.postList,
    friendList = user.friendList)

  implicit def toProfile(user: User): Profile = Profile(username =
    user.username, about = user.about)

  implicit def toTimeline(user: User): TimeLine = TimeLine(username =
    user.username, postList = user.postList)

  implicit def toPosts(post_List: List[Post]): List[Post] = post_List

  implicit def toFriend(user: User): Friend = Friend(username =
    user.username, friendList = user.friendList)

  implicit def toFriends(friend_List: List[String]): List[String] = friend_List

  implicit def toUsersName(user: User): String = user.username

  implicit def toPublicKey(publicKey: PublicKey): PublicKey = publicKey
}
