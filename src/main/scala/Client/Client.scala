//package Client
//
//import com.typesafe.config.ConfigFactory
//
///**
//  * Created by Priti Changlani on 11/22/15 at 4:41 PM.
//  */
//class Client {
//
//  def main(args: Array[String]): Unit = {
//
//    val configfactory = ConfigFactory.parseString(
//      """
//    akka {
//      loglevel = "ERROR"
//      actor {
//        provider = "akka.remote.RemoteActorRefProvider"
//      }
//      remote {
//        enabled-transports = ["akka.remote.netty.tcp"]
//        //log-sent-messages = on
//        //log-received-messages = on
//        netty.tcp {
//          hostname = "127.0.0.1"
//          port = 0
//        }
//      }
//    }
//      """
//    )
//  }
//
//}
