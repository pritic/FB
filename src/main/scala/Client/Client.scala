package Client

import java.security._
import java.security.spec.X509EncodedKeySpec
import java.util
import java.util.concurrent._
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

import scala.collection.convert.decorateAsScala._
import scala.collection.immutable._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by Priti Changlani on 11/22/15 at 4:41 PM.
  */
object Client {

  val userIDList = new ConcurrentSkipListSet[String]().asScala
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

    val b = System.currentTimeMillis()

    for (i <- 0 until numOfUsers) {
      userIDList.add("user" + i)

      val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
      keyPairGenerator.initialize(2048)
      val keyPair: KeyPair = keyPairGenerator.genKeyPair()
      val publicKeyBytes = keyPair.getPublic
      val privateKeyBytes = keyPair.getPrivate

      val client = system.actorOf(Props(new FBUser("user" + i, "name" + i, "about" + i, publicKeyBytes, privateKeyBytes, serverIP, serverPort, system)), "user" + i.toString)

      client ! CreateUser
      Thread.sleep(10)

      val user1: String = "user" + Random.nextInt(userIDList.size)

      system.scheduler.scheduleOnce(10000 millis, client, Schedule(user1))
    }
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
    var postList: scala.collection.immutable.Map[String, Post] = new scala.collection.immutable
    .HashMap[String, Post]
    var friendList: scala.collection.immutable.Set[String] = new scala.collection.immutable
    .HashSet[String]
    val privateKey: PrivateKey = privateKey1
    val publicKey: PublicKey = publicKey1
    var profileKey: String = ""
    var postAESKeyMap: scala.collection.immutable.Map[String, String] = new scala.collection.immutable.HashMap[String, String]

    def hex_Digest(s: String): String = {

      val sha = MessageDigest.getInstance("SHA-256")
      sha.digest(s.getBytes).foldLeft("")((s: String, b: Byte) =>
        s + Character.forDigit((b & 0xf0) >> 4, 16) + Character.forDigit(b & 0x0f, 16))
    }

    def getSecureRandom: String = {

      val random: SecureRandom = SecureRandom.getInstance("SHA1PRNG")
      val byteArray: Array[Byte] = new Array[Byte](32)
      val value1 = random.nextBytes(byteArray)
//      val value = random.nextInt()
      val shaoutput = hex_Digest(value1.toString)
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

    //    def getPublicKey(id1: String): PublicKey = {
    //
    //      implicit val resolveTimeout = Timeout(10 seconds)
    //      val actorRef = Await.result(system.actorSelection("/user/" + id1)
    //        .resolveOne(), 20 second)
    //
    //      val future2 = ask(actorRef, GetPublicKey).mapTo[PublicKey]
    //      val result2 = Await.result(future2, 10 second)
    //      result2
    //    }

    def getAESKey(askerID: String, id1: String, date1: String): String = {

      implicit val resolveTimeout = Timeout(10 seconds)
      val actorRef = Await.result(system.actorSelection("/user/" + id1).resolveOne(), 20 second)

      val future2 = ask(actorRef, GetAESKeyForPost(askerID, date1)).mapTo[String]
      val result2 = Await.result(future2, 10 second)
      result2

    }

    def getProfileKey(askerID: String, id1: String): String = {

      implicit val resolveTimeout = Timeout(10 seconds)
      val actorRef = Await.result(system.actorSelection("/user/" + id1).resolveOne(), 20 second)

      val future2 = ask(actorRef, GetProfileKeyAES(askerID)).mapTo[String]
      val result2 = Await.result(future2, 10 second)
      result2
    }

    def authenticate(): String = {

      implicit val timeout = Timeout(10 seconds)
      val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" + serverIP + ":" + serverPort + "/authenticate/" + id))).mapTo[HttpResponse]

      val response = Await.result(future, timeout.duration)

      val answer = decryptRSAPrivate(privateKey, response.entity.asString)

      val future1 = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" + serverIP + ":" + serverPort + "/checkAnswer?sender=" + id + "&answer=" + answer))).mapTo[HttpResponse]

      val response1 = Await.result(future1, timeout.duration)

      println("response: Authentication: " + response1.entity.asString)

      response1.entity.asString
    }

    def getPublicKeyFromServer(ofUser: String): PublicKey = {

      implicit val timeout = Timeout(10 seconds)
      val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" + serverIP + ":" + serverPort + "/givePublicKey/" + ofUser))).mapTo[HttpResponse]

      val response = Await.result(future, timeout.duration)

      val publicBytes: Array[Byte] = Base64.decodeBase64(response.entity.asString)
      val keySpec: X509EncodedKeySpec = new X509EncodedKeySpec(publicBytes)
      val keyFactory: KeyFactory = KeyFactory.getInstance("RSA")
      val publicKey: PublicKey = keyFactory.generatePublic(keySpec)

      publicKey
    }

    def receive: Receive = {

      //      case GetAESKeyForPost(id1: String, date1: String) => {
      //
      //        var e_Pub_self_AES: String = ""
      //
      //        postAESKeyMap.get(date1) match {
      //          case Some(x) =>
      //            e_Pub_self_AES = x
      //          case None =>
      //            println("Key Not found")
      //        }
      //
      //        var requesters_public_key: PublicKey = null
      //
      //        publicKeys.get(id1) match {
      //          case Some(x) =>
      //            requesters_public_key = x
      //          case None =>
      //            println("Key Not found")
      //
      //        }
      //        sender ! encryptRSAPublic(requesters_public_key, decryptRSAPrivate(privateKey, e_Pub_self_AES))
      //      }
      //      case GetPublicKey =>
      //        sender ! publicKey
      //
      //      case GetProfileKeyAES(askerID: String) =>
      //
      //        var askersPublicKey: PublicKey = null
      //
      //        publicKeys.get(askerID) match {
      //          case Some(x) =>
      //            askersPublicKey = x
      //        }
      //
      //        sender ! encryptRSAPublic(askersPublicKey, profileKey)

      case Schedule(id1: String) =>

        //        self ! getPublicKeyFromServer(id1)

        //        if (!id.equalsIgnoreCase(id1))
        //          self ! MakeFriend(id1)
        //        self ! GetProfile(id1)
        //        self ! GetFriendList(id1)
        var randomuser = "user" + Random.nextInt(userIDList.size)
        self ! PostMessage(randomuser, "This is a post from " + id + " to " + randomuser + " - random post", "public")
        randomuser = "user" + Random.nextInt(userIDList.size)
        self ! PostMessage(randomuser, "This is a post from " + id + " to " + randomuser + " - random post", "public")
        self ! PostMessage(id1, "This is a post from " + id + " to " + id1, privacyList.get(Random.nextInt(3)))
        self ! PostMessage(id1, "This is a post from " + id + " to " + id1, privacyList.get(Random.nextInt(3)))
        self ! PostMessage(id1, "This is a post from " + id + " to " + id1, privacyList.get(Random.nextInt(3)))
        //
        //        self ! PostPicture("album1", privacyList.get(Random.nextInt(2)))
        //        self ! PostPicture("album2", privacyList.get(Random.nextInt(2)))
        self ! GetMyPosts
      //        self ! GetTimeline(id1)
      //        self ! GetPictures(id1)
      //        self ! GetAlbum(id1, "album1")
      //        self ! GetAlbum(id1, "album2")

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
        println("response: CreateUser: " + id + " " + response.entity.asString)

      case GetProfile(id1: String) =>

        if (authenticate().equals("true")) {
          implicit val timeout = Timeout(10 seconds)
          val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" + serverIP + ":" + (serverPort) + "/profile/" + id1))).mapTo[HttpResponse]

          val response = Await.result(future, timeout.duration)

          val myObject: JSONObject = new JSONObject(response.entity.asString)

          if (!id.equalsIgnoreCase(id1)) {
            val encryptedAESKey = getProfileKey(id, id1)

            val decryptedAESKey = decryptRSAPrivate(privateKey, encryptedAESKey)

            println(id + " requested the below profile:\nName: " + decryptAES(decryptedAESKey, "RandomInitVector", myObject.getString("username")) + "\nAbout: " + decryptAES(decryptedAESKey, "RandomInitVector", myObject.getString("about")))
          }
          else {
            println(id + " requested the below profile:\nName: " + decryptAES(profileKey, "RandomInitVector", myObject.getString("username")) + "\nAbout: " + decryptAES(profileKey, "RandomInitVector", myObject.getString("about")))
          }
        }
        else
          println("You are not an authorized user")

      //      case GetTimeline(id1: String) =>
      //        if (authenticate().equals("true")) {
      //          implicit val timeout = Timeout(10 seconds)
      //
      //          val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" + serverIP + ":" + serverPort + "/timeline?sender=" + id + "&requested=" + id1))).mapTo[HttpResponse]
      //
      //          val response = Await.result(future, timeout.duration)
      //
      //          //          println("response: GetTimeline raw: " + response.entity.asString)
      //
      //          val responseList: JSONArray = new JSONArray(response.entity.asString)
      //
      //          val i: Int = responseList.length()
      //
      //          for (x <- 0 until i) {
      //
      //            if (responseList.getJSONObject(x).getString("from").equalsIgnoreCase(id)) {
      //              var encryptedKey: String = ""
      //              postAESKeyMap.get(responseList.getJSONObject
      //              (x).getString("date")) match {
      //                case Some(x) =>
      //                  encryptedKey = x
      //                case None =>
      //                  println("Key Not found")
      //              }
      //              val decryptedKey = decryptRSAPrivate(privateKey, encryptedKey)
      //
      //              println("Decrypted Timeline Post#" + (x + 1) + "\n" + "From: " + responseList.getJSONObject
      //              (x).getString("from") + "\nTo: " + responseList.getJSONObject(x).getString("to") + "\nDate: " + responseList.getJSONObject(x).getString("date") + "\nContent: " + decryptAES(decryptedKey, "RandomInitVector", responseList.getJSONObject(x).getString("content")))
      //            }
      //
      //            else {
      //              val encryptedKey = getAESKey(id, responseList.getJSONObject(x).getString("from"), responseList.getJSONObject(x).getString("date"))
      //
      //              val decryptedKey = decryptRSAPrivate(privateKey, encryptedKey)
      //
      //              println("Decrypted Timeline Post#" + (x + 1) + "\n" + "From: " + responseList.getJSONObject(x).getString("from") + "\nTo: " + responseList.getJSONObject(x).getString("to") + "\nDate: " + responseList.getJSONObject(x).getString("date") + "\nContent: " + decryptAES(decryptedKey, "RandomInitVector", responseList.getJSONObject(x).getString("content")))
      //            }
      //          }
      //        }
      //        else
      //          println("You are not an authorized user")

      case PostMessage(friendID: String, content: String, privacy: String) =>

        if (authenticate().equals("true")) {
          val key = getSecureRandom

          val encryptedContent = encryptAES(key, "RandomInitVector", content)

          val date1 = System.currentTimeMillis().toString

          implicit val timeout = Timeout(10 seconds)
          val encryptedAESFromKey = encryptRSAPublic(publicKey, key)
          val keyDataFrom = new KeyData(date1, encryptedAESFromKey).toJson

          val future0 = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" + serverIP + ":" + serverPort + "/postFromMap")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), keyDataFrom.toString()))).mapTo[HttpResponse]

          val response0 = Await.result(future0, timeout.duration)

          println("response: PostMessage: response0: " + response0.entity.asString)

          val friendsPublicKey = getPublicKeyFromServer(friendID)
          val encryptedAESToKey = encryptRSAPublic(friendsPublicKey, key)
          val keyDataTo = new KeyData(date1, encryptedAESToKey).toJson

          val future1 = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" + serverIP + ":" + serverPort + "/postToMap")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), keyDataTo.toString()))).mapTo[HttpResponse]

          val response1 = Await.result(future1, timeout.duration)

          println("response: PostMessage: response1: " + response1.entity.asString)

          val postJSON = new Post(date1, id, friendID, privacy, encryptedContent).toJson

          val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" + serverIP + ":" + serverPort + "/post")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), postJSON.toString()))).mapTo[HttpResponse]

          val response = Await.result(future, timeout.duration)

          println("response: PostMessage by " + id + " to " + friendID + ": " + response.entity.asString)
        }
        else
          println("You are not an authorized user")

      case GetMyPosts =>
        if (authenticate().equals("true")) {
          implicit val timeout = Timeout(10 seconds)
          val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" + serverIP + ":" + serverPort + "/post/" + id))).mapTo[HttpResponse]

          val response = Await.result(future, timeout.duration)

          val responseList: JSONArray = new JSONArray(response.entity.asString)

          println("Encrypted Posts: "+responseList)

          val i: Int = responseList.length()

          for (x <- 0 until i) {

            if (responseList.getJSONObject(x).getString("from").equalsIgnoreCase(id)) {

              val future0 = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" + serverIP + ":" + serverPort + "/getFromKey?date=" + responseList.getJSONObject(x).getString("date")))).mapTo[HttpResponse]

              //AES key encrypted with sender's public key
              val response0 = Await.result(future0, timeout.duration)

              val decryptedKey = decryptRSAPrivate(privateKey, response0.entity.asString)

              println("Response GetMyPost Decrypted Post:\n" + "From: " + responseList.getJSONObject(x).getString("from") + "\nTo: " + responseList.getJSONObject(x).getString("to") + "\nDate: " + responseList.getJSONObject(x).getString("date") + "\nContent: " + decryptAES(decryptedKey, "RandomInitVector", responseList.getJSONObject(x).getString("content")))
            }

            else {

              val future1 = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" + serverIP + ":" + serverPort + "/getToKey?date=" + responseList.getJSONObject(x).getString("date")))).mapTo[HttpResponse]

              //AES key encrypted with recepient's public key
              val response1 = Await.result(future1, timeout.duration)

              val decryptedKey = decryptRSAPrivate(privateKey, response1.entity.asString)

              println("Response GetMyPost Decrypted Post:\n" + "From: " + responseList.getJSONObject(x).getString("from") + "\nTo: " + responseList.getJSONObject(x).getString("to") + "\nDate: " + responseList.getJSONObject(x).getString("date") + "\nContent: " + decryptAES(decryptedKey, "RandomInitVector", responseList.getJSONObject(x).getString("content")))
            }
          }
        }
        else
          println("You are not an authorized user")

      case MakeFriend(id1: String) =>
        if (authenticate().equals("true")) {
          implicit val timeout = Timeout(10 seconds)

          val friendRequestJSON = new FriendRequest(id, id1).toJson

          val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" + serverIP + ":" + serverPort + "/makefriend")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), friendRequestJSON.toString()))).mapTo[HttpResponse]

          val response = Await.result(future, timeout.duration)

          println("response: MakeFriend: " + id + " is making friends with " + id1 + " " + response.entity.asString)
        }
        else
          println("You are not an authorized user")

      case GetFriendList(id1: String) =>
        if (authenticate().equals("true")) {
          implicit val timeout = Timeout(10 seconds)

          val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" + serverIP + ":" + serverPort + "/getfriends/" + id1))).mapTo[HttpResponse]

          val response = Await.result(future, timeout.duration)

          println("response: GetFriendList: " + id + " is getting " + id1 + "'s friends " + response.entity.asString)
        }
        else
          println("You are not an authorized user")

      case PostPicture(albumID: String, privacy: String) =>

        if (authenticate().equals("true")) {
          val is = this.getClass.getClassLoader().getResourceAsStream("picture.jpg");
          val stream = Stream.continually(is.read).takeWhile(_ != -1).map(_.toByte)
          val bytes = stream.toArray
          val pictureBase64String = new sun.misc.BASE64Encoder().encode(bytes)
          val key = getSecureRandom
          val encryptedPictureBase64String = encryptAES(key, "RandomInitVector", pictureBase64String)
          val date1 = System.currentTimeMillis().toString

          postAESKeyMap += (date1 -> encryptRSAPublic(publicKey, key))

          val imageJSON = new Picture(date1, id, albumID, privacy, encryptedPictureBase64String)
            .toJson
          implicit val timeout = Timeout(10 seconds)
          val future = IO(Http)(system).ask(HttpRequest(POST, Uri(s"http://" +
            serverIP + ":" + serverPort + "/picture")).withEntity(HttpEntity(ContentType(MediaTypes.`application/json`), imageJSON.toString()))).mapTo[HttpResponse]
          val response = Await.result(future, timeout.duration)
          println("response: PostPicture: " + response.entity.asString)
        }
        else {
          println("You are not an authorized user")
        }

      case GetPictures(id1: String) =>

        if (authenticate().equals("true")) {
          implicit val timeout = Timeout(10 seconds)
          val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
            serverIP + ":" + serverPort + "/picture?sender=" +
            id1 + "&requested=" + id))).mapTo[HttpResponse]
          val response = Await.result(future, timeout.duration)
          val responseList: JSONArray = new JSONArray(response.entity.asString)
          //          println("List of Pictures: " + responseList)

          val i: Int = responseList.length()

          for (x <- 0 until i) {
            if (responseList.getJSONObject(x).getString("from").equalsIgnoreCase(responseList.getJSONObject(x).getString("to")) || responseList.getJSONObject(x).getString("from").equalsIgnoreCase(id)) {
              var encryptedKey: String = ""
              postAESKeyMap.get(responseList.getJSONObject
              (x).getString("date")) match {
                case Some(x) =>
                  encryptedKey = x
                case None =>
                  println("Key Not found")
              }
              val decryptedKey = decryptRSAPrivate(privateKey, encryptedKey)

              println("Picture:\n" + "From: " + responseList.getJSONObject
              (x).getString("from") + "\nAlbum: " + responseList
                .getJSONObject
                (x).getString("to") + "\nRequestedBy: " + id + "\nDate: " + responseList.getJSONObject
              (x).getString("date") + "\nContent: " + decryptAES(decryptedKey, "RandomInitVector", responseList.getJSONObject(x).getString("content")))
            }
            else {
              val encryptedKey = getAESKey(id, responseList.getJSONObject
              (x).getString("from"), responseList.getJSONObject
              (x).getString("date"))

              val decryptedKey = decryptRSAPrivate(privateKey, encryptedKey)
              println("Post:\n" + "From: " + responseList.getJSONObject
              (x).getString("from") + "\nTo: " + responseList
                .getJSONObject
                (x).getString("to") + "\nRequestedBy: " + id + "\nDate: " + responseList.getJSONObject
              (x).getString("date") + "\nContent: " + decryptAES(decryptedKey, "RandomInitVector", responseList.getJSONObject(x).getString("content")))
            }
          }
        }
        else {
          println("You are not an authorized user")
        }

      case GetAlbum(requestorID: String, albumID: String) =>

        if (authenticate().equals("true")) {
          implicit val timeout = Timeout(10 seconds)
          val future = IO(Http)(system).ask(HttpRequest(GET, Uri(s"http://" +
            serverIP + ":" + serverPort + "/album?albumowner=" + requestorID + "&requestorid=" +
            id + "&requestedalbum=" + albumID))).mapTo[HttpResponse]
          val response = Await.result(future, timeout.duration)
          val responseList: JSONArray = new JSONArray(response.entity.asString)
          //          println("List of Pictures from specific Album: " + responseList)

          val i: Int = responseList.length()

          for (x <- 0 until i) {

            if (responseList.getJSONObject
            (x).getString("from").equalsIgnoreCase(id)) {
              var encryptedKey: String = ""
              postAESKeyMap.get(responseList.getJSONObject
              (x).getString("date")) match {
                case Some(x) =>
                  encryptedKey = x
                case None =>
                  println("Key Not found")
              }
              val decryptedKey = decryptRSAPrivate(privateKey, encryptedKey)

              println("Album:\n" + "From: " + responseList.getJSONObject
              (x).getString("from") + "\nAlbum: " + responseList
                .getJSONObject
                (x).getString("to") + "\nRequestedBy: " + id + "\nDate: " + responseList.getJSONObject
              (x).getString("date") + "\nContent: " + decryptAES(decryptedKey, "RandomInitVector", responseList.getJSONObject(x).getString("content")))
            }
            else {
              val encryptedKey = getAESKey(id, responseList.getJSONObject
              (x).getString("from"), responseList.getJSONObject
              (x).getString("date"))

              val decryptedKey = decryptRSAPrivate(privateKey, encryptedKey)
              println("Post:\n" + "From: " + responseList.getJSONObject
              (x).getString("from") + "\nTo: " + responseList
                .getJSONObject
                (x).getString("to") + "\nRequestedBy: " + id + "\nDate: " + responseList.getJSONObject
              (x).getString("date") + "\nContent: " + decryptAES(decryptedKey, "RandomInitVector", responseList.getJSONObject(x).getString("content")))
            }
          }
        }
        else {
          println("You are not an authorized user")
        }
    }
  }

}

case class GetAESKeyForPost(id1: String, date1: String)

case class GetPublicKey()

case class GetProfileKeyAES(askerID: String)

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