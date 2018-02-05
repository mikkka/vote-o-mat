package name.mtkachev.voteomat.domain

import java.util.Date

sealed trait Command

case class CreateVoting(
                         name: String,
                         anon: Boolean,
                         viewAfterStop: Boolean,
                         startDate: Option[Date],
                         stopDate: Option[Date]
                       ) extends Command

case class ListVotings() extends Command
case class DeleteVoting(votingId: Int) extends Command
case class StartVoting(votingId: Int) extends Command
case class StopVoting(votingId: Int) extends Command

case class ViewVotingResult(votingId: Int) extends Command

sealed trait AddQuestion
case class AddOpenQuestion(question: String) extends Command with AddQuestion
case class AddCloseQuestion(
                            question: String,
                            options: List[String]
                          ) extends Command with AddQuestion
case class AddMultiQuestion(
                             question: String,
                             options: List[String]
                           ) extends Command with AddQuestion


case class DeleteQuestion(questionId: Int) extends Command

case class Vote(
                 questionId: Int,
                 answer: String,
                 answerDig: List[Int]) extends Command

sealed case class CommandContext(userId: String, votingId: Option[Int])