package name.mtkachev.akkastreamsexps.tcpecho

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl.Tcp._
import akka.stream.scaladsl._
import akka.util.ByteString

import scala.concurrent.Future

object TcpEchoApp2 extends App {
  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val connections: Source[IncomingConnection, Future[ServerBinding]] =
    Tcp().bind("127.0.0.1", 8888)

  var cmdCounter = 0

  connections.runForeach { connection =>
    val g = Flow.fromGraph(GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      val inFlow = Flow[ByteString]
        .via(
          Framing.delimiter(ByteString("\n"),
            maximumFrameLength = 256,
            allowTruncation = true))
        .map(_.utf8String)

      val in = b.add(inFlow)
      val broadcast = b.add(Broadcast[String](2))
      val merge = b.add(Merge[String](2))

      val toBytes = Flow[String].map(_ + "\n").map(ByteString(_))
      val out = b.add(toBytes)

      in.out ~> broadcast.in
      broadcast.out(0) ~> merge
      broadcast.out(1) ~> merge

      merge ~> out

      FlowShape(in.in, out.out)
    })

    connection.handleWith(g)
  }
}
