package name.mtkachev.voteomat.domain

import java.util.Date

sealed trait Command

case class CreateVoting(
                         userId: String,
                         name: String,
                         anon: Boolean,
                         viewAfterStop: Boolean,
                         startDate: Option[Date],
                         stopDate: Option[Date]
                       ) extends Command
case class ListVotings() extends Command
case class DeleteVoting(userId: String, votingId: Int) extends Command
case class StartVoting(userId: String, votingId: Int) extends Command
case class StopVoting(userId: String, votingId: Int) extends Command

case class ViewVotingResult(userId: String, votingId: Int) extends Command

sealed trait AddQuestion
case class AddOpenQuestion(
                        userId: String,
                        votingId: Int,
                        question: String
                      ) extends Command with AddQuestion
case class AddCloseQuestion(
                            userId: String,
                            votingId: Int,
                            question: String,
                            options: List[String]
                          ) extends Command with AddQuestion
case class AddMultiQuestion(
                             userId: String,
                             votingId: Int,
                             question: String,
                             options: List[String]
                           ) extends Command with AddQuestion


case class DeleteQuestion(userId: String, votingId: Int, questionId: Int) extends Command

sealed trait Vote
case class OpenVote(
                     userId: String,
                     votingId: Int,
                     answer: String) extends Command with Vote

case class CloseVote(
                      userId: String,
                      votingId: Int,
                      answer: Int
                    ) extends Command with Vote

case class MultiVote(
                      userId: String,
                      votingId: Int,
                      answer: List[Int]
                    ) extends Command with Vote

sealed case class CommandContext(userId: String, votingId: Int)