package name.mtkachev.voteomat.domain

import java.time.LocalDateTime
import java.util.Date

sealed trait Command
sealed trait CommonCommand extends Command
sealed case class CommonCommandContext(userId: String, votingId: Option[Int])

sealed trait CtxCommand extends Command

case class CreateVoting(
                         name: String,
                         anon: Boolean,
                         viewAfterStop: Boolean,
                         startDate: Option[LocalDateTime],
                         stopDate: Option[LocalDateTime]
                       ) extends CommonCommand

case class ListVotings() extends CommonCommand
case class DeleteVoting(votingId: Int) extends CommonCommand
case class StartVoting(votingId: Int) extends CommonCommand
case class StopVoting(votingId: Int) extends CommonCommand

case class ViewVotingResult(votingId: Int) extends CommonCommand

case class ViewVoting() extends CommonCommand

sealed trait AddQuestion extends CommonCommand
case class AddOpenQuestion(question: String) extends AddQuestion
case class AddCloseQuestion(
                            question: String,
                            options: List[String]
                          ) extends AddQuestion
case class AddMultiQuestion(
                             question: String,
                             options: List[String]
                           ) extends AddQuestion


case class DeleteQuestion(questionId: Int) extends CommonCommand

case class Vote(
                 questionId: Int,
                 answer: String) extends CommonCommand

case class Begin(votingId: Int) extends CtxCommand
case class End() extends CtxCommand