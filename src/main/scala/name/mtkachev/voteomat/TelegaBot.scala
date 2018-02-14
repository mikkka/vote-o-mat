package name.mtkachev.voteomat

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

object TelegaBot extends App {
  val token = args(0)
  class VoteBot(val token: String) extends TelegramBot with Polling {
    override def receiveMessage(msg: Message): Unit = {
      for (text <- msg.text)
        request(SendMessage(msg.source, text.reverse))
    }
  }

  new VoteBot(token).run()
}
