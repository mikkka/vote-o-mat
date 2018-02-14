package name.mtkachev.voteomat

import java.time.LocalDateTime

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, FlowShape}
import akka.stream.scaladsl.{Broadcast, Flow, Framing, GraphDSL, Merge, Sink, Source, Tcp}
import akka.stream.scaladsl.Tcp.{IncomingConnection, ServerBinding}
import akka.util.ByteString
import name.mtkachev.voteomat.domain._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object TelnetApp {
  def main(args: Array[String]): Unit = {
    println("hello!")
    implicit val actorSystem = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val commandParser = new SimpleCommandParser {}
    val stateCtx = new VoteStateCtx
    val resultsRenderer = new ResultsRendererImpl

    val connections: Source[IncomingConnection, Future[ServerBinding]] =
      Tcp().bind("127.0.0.1", 8888)

    connections.runForeach { connection =>
      val sessionCtx = new SessionCtx

      import connection._
      val welcomeMsg =
        s"Welcome to: $localAddress, you is is: $remoteAddress! Predstavsya mraz: "
      val welcome = Source.single(welcomeMsg)

      val g: Flow[String, String, NotUsed] = Flow.fromGraph(GraphDSL.create() { implicit b =>
        import GraphDSL.Implicits._

        val commandParserFlow = Flow[String].map{x =>
          if (sessionCtx.isEmpty) Success(User(x))
          else commandParser.parse(x)
        }

        val in = b.add(commandParserFlow)
        val broadcast = b.add(Broadcast[Try[Command]](3))
        val merge = b.add(Merge[Try[ApplyRes]](3))

        val toString = Flow[Try[ApplyRes]]
          .map(resultsRenderer.render)
          .merge(welcome)

        val out = b.add(toString)

        in.out ~> broadcast.in

        //there go session ctx ctrl
        broadcast.out(0) ~>
          Flow[Try[Command]]
            .collect {
              case Success(cmd: CtxCommand) => sessionCtx.applyCmd(cmd)
            } ~> merge

        //there go cmds
        broadcast.out(1) ~>
          Flow[Try[Command]]
            .collect {
              case Success(cmd: CommonCommand) =>
                Try(sessionCtx.get).flatMap { ctx =>
                  stateCtx.applyCmd(ctx, cmd)
                }
            } ~> merge

        //there go parse errors
        broadcast.out(2) ~>
          Flow[Try[Command]].collect {
            case x @ Failure(_) => x.asInstanceOf[Failure[ApplyRes]]
          } ~> merge

        merge ~> out.in

        FlowShape(in.in, out.out)
      })

      val logic = Flow[ByteString]
        .via(
          Framing.delimiter(ByteString("\n\n"),
            maximumFrameLength = 256,
            allowTruncation = true))
        .map(_.utf8String)
        .via(g).map(x => ByteString(x + "\n" + ("-" * 30) + "\n"))


      connection.handleWith(logic)
    }
  }
}
