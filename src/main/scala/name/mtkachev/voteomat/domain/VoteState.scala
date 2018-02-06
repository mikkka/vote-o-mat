package name.mtkachev.voteomat.domain

import java.time.LocalDateTime

case class VoteState (
  votings: Map[Int,Voting]
)

case class User (name: String)

case class Voting (
  id: Int,
  owner: User,
  name: String,
  isAnonymous: Boolean,
  resultsAfterStop: Boolean,
  startDate: Option[LocalDateTime],
  stopDate: Option[LocalDateTime],
  questions: List[Question]
)

sealed trait Answer {
  def author: Option[User]
  def timeDate: LocalDateTime
}

sealed trait Question {
  type A <: Answer

  def id: Int
  def label: String
  def answers: List[A]
}

case class OpenQuestion (
  override val id: Int,
  override val label: String,
  override val answers: List[OpenAnswer]
) extends Question {
  type A = OpenAnswer
}

case class OpenAnswer (
  override val author: Option[User],
  override val timeDate: LocalDateTime,
  value: String
) extends Answer


case class CloseQuestion (
  override val id: Int,
  override val label: String,
  options: List[String],
  override val answers: List[CloseAnswer]
) extends Question {
  type A = CloseAnswer
}

case class CloseAnswer (
  override val author: Option[User],
  override val timeDate: LocalDateTime,
  value: Int
) extends Answer

case class MultiQuestion (
  override val id: Int,
  override val label: String,
  options: List[String],
  override val answers: List[MultiAnswer]
) extends Question {
  type A = MultiAnswer
}

case class MultiAnswer (
  override val author: Option[User],
  override val timeDate: LocalDateTime,
  value: Set[Int]
) extends Answer
