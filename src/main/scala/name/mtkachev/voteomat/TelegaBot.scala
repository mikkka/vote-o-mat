package name.mtkachev.voteomat

import akka.stream.scaladsl.Source
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.collection.concurrent.TrieMap
import domain.{User => UserCmd}

object TelegaBot extends App {
  val token = args(0)
  class VoteBot(val token: String) extends TelegramBot with Polling {
    val welcome = Source.empty
    val stateCtx = new VoteStateCtx
    val sessions = TrieMap.empty[String, SessionCtx]

    override def receiveMessage(msg: Message): Unit = {
      for {
        text        <- msg.text
        user        <- msg.from
        userName    <- user.username
      } {
        val sess = sessions.getOrElseUpdate(userName, new SessionSingleCtx)
        sess.applyCmd(UserCmd(userName))

        val logic = LogicGraph(stateCtx, sess, welcome)
        Source.single(text).via(logic).take(1).runForeach(res =>
          request(SendMessage(msg.source, res))
        )
      }
    }
  }

  new VoteBot(token).run()
}
