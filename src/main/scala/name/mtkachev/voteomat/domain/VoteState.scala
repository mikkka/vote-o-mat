package name.mtkachev.voteomat.domain

import java.time.LocalDateTime

import scala.util.Try

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
  isManualStarted: Boolean,
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


import name.mtkachev.voteomat.util.LensT
import name.mtkachev.voteomat.util.LensT._

import cats.instances.try_._

object VoteState {

  val addVoting = (s: VoteState, v: Voting) =>
    Try {
      assert(!s.votings.contains(v.id))
      s.copy(votings = s.votings + (v.id -> v))
    }

  val deleteVoting = (s: VoteState, id: Int, timeDate: LocalDateTime) =>
    Try {
      val v = s.votings(id)
      Voting.assertNotFreezed(v, timeDate)
      s.copy(votings = s.votings - id)
    }

  def votingLens(id: Int): LensT[Try, VoteState, Voting] = lens1(
    s => Try(s.votings(id)),
    (s, v) => s.copy(votings = s.votings + (id -> v.copy(id = id)))
  )
}

object Voting {
  def isFreezed(v: Voting, timeDate: LocalDateTime): Boolean =
    v.isManualStarted || v.startDate.exists(timeDate.isAfter)

  def isResultCanBeViewed(v: Voting, timeDate: LocalDateTime): Boolean =
    !v.isManualStarted || v.stopDate.exists(timeDate.isAfter)

  def assertNotFreezed(v: Voting, timeDate: LocalDateTime): Unit =
    assert(!isFreezed(v, timeDate))

  def assertResultCanBeViewed(v: Voting, timeDate: LocalDateTime): Unit =
    assert(isResultCanBeViewed(v, timeDate))

  def manualStartedLens(timeDate: LocalDateTime) = lens0[Try, Voting, Boolean](
    o => o.isManualStarted,
    (o, v) => Try {
      if (!o.isManualStarted && v) {
        // manual start
        assertNotFreezed(o, timeDate)
        assert(o.startDate.isEmpty)
        o.copy(isManualStarted = v)
      } else if (o.isManualStarted && !v) {
        // manual stop
        assert(o.stopDate.isEmpty)
        o.copy(isManualStarted = v)
      } else o
    }
  )
}