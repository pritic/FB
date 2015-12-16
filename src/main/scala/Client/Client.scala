package Client

import java.security._
import java.util
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import Common.Common._
import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.apache.commons.codec.binary.Base64
import org.json.{JSONArray, JSONObject}
import spray.can.Http
import spray.http.HttpMethods._
import spray.http._
import spray.httpx.unmarshalling.FormDataUnmarshallers
import spray.json._

import scala.collection.immutable._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by Priti Changlani on 11/22/15 at 4:41 PM.
  */
object Client {

  val userIDList: util.HashSet[String] = new util.HashSet[String]()
  val privacyList: util.HashMap[Int, String] = new util.HashMap[Int, String]()
  privacyList.put(0, "friends")
  privacyList.put(1, "public")
  privacyList.put(2, "private")

  var publicKeys: Map[String, PublicKey] = new scala.collection.immutable
  .HashMap[String, PublicKey]

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

    val b = System.currentTimeMillis()

    for (i <- 0 to numOfUsers - 2) {
      userIDList.add("user" + i)

      val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA");
      keyPairGenerator.initialize(2048)
      val keyPair: KeyPair = keyPairGenerator.genKeyPair()
      val publicKeyBytes = keyPair.getPublic
      publicKeys += "user" + i -> publicKeyBytes
      val privateKeyBytes = keyPair.getPrivate

      val client = system.actorOf(Props(new FBUser("user" + i, "name" + i,
        "about" + i, publicKeyBytes, privateKeyBytes, serverIP, serverPort,
        system)), i.toString)
      Thread.sleep(10)

      client ! CreateUser

      //      system.scheduler.scheduleOnce(10000 millis, client, Schedule
      //      ("user" + Random.nextInt(userIDList.size())))

      //      client ! PrintKeys
    }

    val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA");
    keyPairGenerator.initialize(2048)
    val keyPair: KeyPair = keyPairGenerator.genKeyPair()
    val publicKeyBytes = keyPair.getPublic
    publicKeys += "user" + numOfUsers -> publicKeyBytes
    val privateKeyBytes = keyPair.getPrivate

    val client = system.actorOf(Props(new FBUser("user" + numOfUsers, "name" + numOfUsers,
      "about" + numOfUsers, publicKeyBytes, privateKeyBytes, serverIP,
      serverPort, system)),
      numOfUsers
        .toString)
    client ! CreateUser
    //    system.scheduler.scheduleOnce(10000 millis, client, Schedule1
    //    ("user" + Random.nextInt(userIDList.size()), b))

    //    client ! PrintKeys

    Thread.sleep(10000)
    implicit val resolveTimeout = Timeout(10 seconds)
    val actorRef = Await.result(system.actorSelection("/user/0")
      .resolveOne(), 10 second)

    actorRef ! GetMyPosts
    //    val future2 = ask(actorRef, GetMyPosts)
    //    Await.result(future2, 10 second)
  }

  class FBUser(
                userID: String,
                name: String,
                aboutMe: String,
                publicKey1: PublicKey,
                privateKey1: PrivateKey,
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
    val privateKey: PrivateKey = privateKey1
    val publicKey: PublicKey = publicKey1
    var profileKey: String = null
    var postAESKeyMap: Map[String, String] = new scala.collection.immutable
    .HashMap[String, String]

    def hex_Digest(s: String): String = {

      val sha = MessageDigest.getInstance("SHA-256")
      sha.digest(s.getBytes).foldLeft("")((s: String, b: Byte) =>
        s + Character.forDigit((b & 0xf0) >> 4, 16) + Character.forDigit(b & 0x0f, 16))
    }

    def getSecureRandom: String = {

      val random: SecureRandom = SecureRandom.getInstance("SHA1PRNG")
      val value = random.nextInt()
      val shaoutput = hex_Digest(value.toString)
      shaoutput.substring(0, 16)
    }

    def encryptAES(key: String, initVector: String, value: String): String = {

      val iv: IvParameterSpec = new IvParameterSpec(initVector.getBytes("UTF-8"))
      val skeySpec: SecretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES")

      val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
      cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv)

      val encrypted: Array[Byte] = cipher.doFinal(value.getBytes())

      Base64.encodeBase64String(encrypted)
    }

    def decryptAES(key: String, initVector: String, encrypted: String): String = {

      val iv: IvParameterSpec = new IvParameterSpec(initVector.getBytes("UTF-8"))
      val skeySpec: SecretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES")

      val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
      cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv)

      val decrypted: Array[Byte] = cipher.doFinal(Base64.decodeBase64
      (encrypted))

      new String(decrypted)
    }

    def encryptRSAPublic(key: PublicKey, value: String): String = {

      val cipher: Cipher = Cipher.getInstance("RSA/ECB/PKCS1PADDING")
      // encrypt the plain text using the public key
      cipher.init(Cipher.ENCRYPT_MODE, key)
      val encrypted: Array[Byte] = cipher.doFinal(value.getBytes())

      Base64.encodeBase64String(encrypted)
    }

    def decryptRSAPrivate(key: PrivateKey, encrypted: String): String = {

      val cipher: Cipher = Cipher.getInstance("RSA/ECB/PKCS1PADDING")

      // decrypt the text using the private key
      cipher.init(Cipher.DECRYPT_MODE, key)
      val decrypted: Array[Byte] = cipher.doFinal(Base64.decodeBase64
      (encrypted))

      new String(decrypted)
    }

    def getPublicKey(id1: String): PublicKey = {

      implicit val resolveTimeout = Timeout(10 seconds)
      val actorRef = Await.result(system.actorSelection("/user/" + id1)
        .resolveOne(), 10 second)

      val future2 = ask(actorRef, GetPublicKey).mapTo[PublicKey]
      val result2 = Await.result(future2, 10 second)
      result2
    }

    //sends AES key encrypted with requester's public key
    def getAESKey(askerID: String, id1: String, date1: String): String = {

      implicit val resolveTimeout = Timeout(10 seconds)
      val actorRef = Await.result(system.actorSelection("/user/" + id1)
        .resolveOne(), 10 second)

      val future2 = ask(actorRef, GetAESKey(askerID, date1)).mapTo[String]
      val result2 = Await.result(future2, 10 second)
      result2

    }

    //sends AES key encrypted with the requested profile user's public key

    def getProfileKey(askerID: String, id1: String): String = {

      implicit val resolveTimeout = Timeout(10 seconds)
      val actorRef = Await.result(system.actorSelection("/user/" + id1)
        .resolveOne(), 10 second)

      val future2 = ask(actorRef, GetProfileKeyAES).mapTo[String]
      val result2 = Await.result(future2, 10 second)

      var askersPublicKey: PublicKey = null

      publicKeys.get(askerID) match {
        case Some(x) =>
          askersPublicKey = x
      }

      encryptRSAPublic(askersPublicKey, result2)

    }

    def receive: Receive = {

      case GetAESKey(id1: String, date1: String) =>

        var e_Pub_self_AES: String = ""

        postAESKeyMap.get(date1) match {
          case Some(x) =>
            e_Pub_self_AES = x
        }

        var requesters_public_key: PublicKey = null
        publicKeys.get("user"+id1) match {
          case Some(x) =>
            requesters_public_key = x
        }
        sender ! encryptRSAPublic(requesters_public_key, decryptRSAPrivate(privateKey, e_Pub_self_AES))

      case GetPublicKey =>
        sender ! publicKey

      case GetProfileKeyAES =>
        sender ! profileKey

      case Schedule(id1: String) =>

        if (!id.equalsIgnoreCase(id1))
          self ! MakeFriend(id1)
        self ! GetProfile(id1)
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

      case Schedule1(id1: String, time: Long) =>

        implicit val timeout = Timeout(10 seconds)

        if (!id.equalsIgnoreCase(id1))
          self ? MakeFriend(id1)
        self ? GetProfile(id1)
        self ? GetFriendList(id1)
        self ? PostMessage(id1, "This is a post from " + id + " to " +
          id1, privacyList.get(Random.nextInt(3)))
        self ? GetMyPosts
        self ? GetTimeline(id1)
        self ? PostPicture("album1", privacyList.get(Random.nextInt(2)))
        self ? PostPicture("album2", privacyList.get(Random.nextInt(2)))
        self ? GetPictures(id1)
        self ? GetAlbum(id1, "album1")
        self ? GetAlbum(id1, "album2")

        Thread.sleep(100)
        self ? PoisonPill
        println("Total time taken : " + (System.currentTimeMillis() - time) + "milliseconds")

      case CreateUser =>

        implicit val timeout = Timeout(10 seconds)

        val key = getSecureRandom
        this.profileKey = key
        val encryptedUsername = encryptAES(key, "RandomInitVector", username)
        val encryptedAbout = encryptAES(key, "RandomInitVector", about)
        val base64String: String = Base64.encodeBase64String(publicKey.getEncoded)

        val userJSON = new User(id, encryptedUsername, encryptedAbout, base64String, postList, friendList).toJson

        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/user")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), userJSON.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)
        println("response: CreateUser: " + response.entity.asString)

        //        if (!id.equalsIgnoreCase("user0"))
        //          println("inside create user for " + id + " result " + getPublicKey("0")
        //            .toString)

        if (!id.equalsIgnoreCase("user0")) {
          //          self ! GetProfile("0")
          self ! PostMessage("user0", "This is a post from " + id + "" +
            " to user0", privacyList.get(Random.nextInt(3)))
        }

      case GetProfile(id1: String) =>

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + (serverPort) + "/profile/" + "user" + id1)))
          .mapTo[HttpResponse]
        val response = Await.result(future, timeout.duration)

        val myObject: JSONObject = new JSONObject(response.entity.asString)

        val encryptedAESKey = getProfileKey(id, id1)

        val decryptedAESKey = decryptRSAPrivate(privateKey, encryptedAESKey)

        println("Requested Profile is:\nName: " + decryptAES(decryptedAESKey,
          "RandomInitVector", myObject.getString("username")) + "\nAbout: " + decryptAES(decryptedAESKey, "RandomInitVector", myObject.getString("about")))

      case GetTimeline(id1: String) =>

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/timeline?sender=" +
          id + "&requested=" + id1)))
        val response = Await.result(future, timeout.duration)
        println("response: GetTimeline: " + response)

      case PostMessage(friendID: String, content: String,
      privacy: String) =>

        val key = getSecureRandom
        val encryptedContent = encryptAES(key, "RandomInitVector", content)
        val date1 = System.currentTimeMillis().toString
        postAESKeyMap += (date1 -> encryptRSAPublic(publicKey, key))

        val postJSON = new Post(date1, id, friendID, privacy, encryptedContent)
          .toJson

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/post")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), postJSON.toString()))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)

        println("response: PostMessage: " + response.entity.asString)

      case GetMyPosts =>

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/post/" + id))).mapTo[HttpResponse]

        val response = Await.result(future, timeout.duration)

//        println("response: GetMyPosts raw: " + response.entity.asString)

        val responseList: JSONArray = new JSONArray(response.entity.asString)

        println("List of Posts: " + responseList)

        val i: Int = responseList.length()

        for (x <- 0 until i) {
          val encryptedKey = getAESKey(id.substring(4), responseList.getJSONObject
          (x).getString("from").substring(4), responseList.getJSONObject
          (x).getString("date"))

          val decryptedKey = decryptRSAPrivate(privateKey, encryptedKey)
          println("Post:\n" + "From: " + responseList.getJSONObject
          (x).getString("from") + "\nTo: " + responseList
            .getJSONObject
          (x).getString("to") + "\nDate: " + responseList.getJSONObject
          (x).getString("date") + "\nContent: " + decryptAES(decryptedKey, "RandomInitVector", responseList.getJSONObject(x).getString("content")))
        }

      case MakeFriend(id1: String) =>

        implicit val timeout = Timeout(10 seconds)
        val friendRequestJSON = new FriendRequest(id, id1).toJson
        val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
          serverIP + ":" + serverPort + "/makefriend")).withEntity
        (HttpEntity(ContentType(MediaTypes.`application/json`), friendRequestJSON.toString()))).mapTo[HttpResponse]
        val response = Await.result(future, timeout.duration)
        println("response: MakeFriend: " + id + " is making friends with " + id1 + " " + response)

      case GetFriendList(id1: String) =>

        implicit val timeout = Timeout(10 seconds)
        val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
          serverIP + ":" + serverPort + "/getfriends/" + id1)))
        val response = Await.result(future, timeout.duration)
        println("response: GetFriendList: " + id + " is getting " + id1 + "'s friends " +
          response)

      case PostPicture(albumID: String, privacy: String) =>

        val is = this.getClass.getClassLoader().getResourceAsStream("picture.jpg");
        val stream = Stream.continually(is.read).takeWhile(_ != -1).map(_.toByte)
        val bytes = stream.toArray
        val pictureBase64String = new sun.misc.BASE64Encoder().encode(bytes)
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

case class GetAESKey(id1: String, date1: String)

case class GetPublicKey()

case class GetProfileKeyAES()

case class Schedule(id: String)

case class Schedule1(id1: String, time: Long)

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