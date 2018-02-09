package name.mtkachev.akkastreamsexps.tcpecho

import akka.stream.scaladsl._
import Tcp._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.util.ByteString

import scala.concurrent.Future

object TcpEchoApp extends App {
  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val connections: Source[IncomingConnection, Future[ServerBinding]] =
    Tcp().bind("127.0.0.1", 8888)

  var cmdCounter = 0

  connections.runForeach { connection =>
    // server logic, parses incoming commands
    val commandParser = Flow[String].takeWhile(_ != "BYE").map(_ + "!")

    import connection._
    val welcomeMsg = s"Welcome to: $localAddress, you is is: $remoteAddress! Predstavsya mraz: "
    val welcome = Source.single(welcomeMsg)

    val countingFlow = Flow[String].map{x => cmdCounter += 1; x}

    val serverLogic = Flow[ByteString]
      .via(
        Framing.delimiter(ByteString("\n"),
                          maximumFrameLength = 256,
                          allowTruncation = true))
      .map(_.utf8String)
      .via(commandParser)
      .via(countingFlow)
      // merge in the initial banner after parser
      .merge(welcome)
      .map(x => s"$cmdCounter: " + x + "\n")
      .map(ByteString(_))

    connection.handleWith(serverLogic)
  }
}
