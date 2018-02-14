package name.mtkachev.voteomat

import akka.NotUsed
import akka.stream.FlowShape
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Merge, Source}
import name.mtkachev.voteomat.domain._

import scala.util.{Failure, Success, Try}

object LogicGraph {
  def apply(stateCtx: VoteStateCtx, sessionCtx: SessionCtx, welcome: Source[String, NotUsed]) = {
    val commandParser = new SimpleCommandParser {}
    val resultsRenderer = new ResultsRendererImpl
    GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      val commandParserFlow = Flow[String].map { x =>
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
          case x@Failure(_) => x.asInstanceOf[Failure[ApplyRes]]
        } ~> merge

      merge ~> out.in

      FlowShape(in.in, out.out)
    }
  }
}
