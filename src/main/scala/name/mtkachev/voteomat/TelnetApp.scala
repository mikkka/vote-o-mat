package name.mtkachev.voteomat

import java.time.LocalDateTime

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, FlowShape}
import akka.stream.scaladsl.{
  Broadcast,
  Flow,
  Framing,
  GraphDSL,
  Merge,
  Sink,
  Source,
  Tcp
}
import akka.stream.scaladsl.Tcp.{IncomingConnection, ServerBinding}
import akka.util.ByteString
import name.mtkachev.voteomat.domain._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object TelnetApp {
  class SessionCtx {
    private val lock = new Object
    private var ctx = Option.empty[CommonCommandContext]

    def applyCmd(cmd: CtxCommand): Try[ApplyRes] = {
      lock.synchronized {
        cmd match {
          case User(user) => ctx = Some(CommonCommandContext(user, None))
          case Begin(id)  => ctx = ctx.map(x => x.copy(votingId = Some(id)))
          case End()      => ctx = ctx.map(x => x.copy(votingId = None))
        }
        Success(CtxChanged())
      }
    }

    def isEmpty: Boolean = lock.synchronized {
      ctx.isEmpty
    }

    def get: CommonCommandContext = lock.synchronized {
      ctx.get
    }
  }

  class VoteStateCtx {
    private val lock = new Object
    private var state = new VoteState(Map.empty)

    def applyCmd(ctx: CommonCommandContext,
                 cmd: CommonCommand): Try[ApplyRes] = {
      val time = LocalDateTime.now()
      lock.synchronized {
        val res = VoteStateManager.apply(state, time, ctx, cmd)
        res.foreach(x => state = x._1)
        res.map(_._2)
      }
    }
  }

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

      val g = Flow.fromGraph(GraphDSL.create() { implicit b =>
        import GraphDSL.Implicits._

        val commandParserFlow = Flow[String].map{x =>
          if (sessionCtx.isEmpty) Success(User(x))
          else commandParser.parse(x)
        }

        val inFlow = Flow[ByteString]
          .via(
            Framing.delimiter(ByteString("\n\n"),
                              maximumFrameLength = 256,
                              allowTruncation = true))
          .map(_.utf8String)
          .via(commandParserFlow)

        val in = b.add(inFlow)
        val broadcast = b.add(Broadcast[Try[Command]](3))
        val merge = b.add(Merge[Try[ApplyRes]](3))

        val toBytes = Flow[Try[ApplyRes]]
          .map(resultsRenderer.render)
          .merge(welcome)
          .map(x => ByteString(x + "\n" + ("-" * 30) + "\n"))
        val out = b.add(toBytes)

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

      connection.handleWith(g)
    }
  }
}
