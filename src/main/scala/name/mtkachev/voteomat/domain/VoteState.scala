package name.mtkachev.voteomat.domain

import java.time.LocalDateTime

import scala.util.Try

case class VoteState(
    votings: Map[Int, Voting]
)

case class User(name: String)

case class Voting(
    id: Int,
    owner: User,
    name: String,
    isAnonymous: Boolean,
    resultsAfterStop: Boolean,
    isManualStarted: Boolean,
    startDate: Option[LocalDateTime],
    stopDate: Option[LocalDateTime],
    questions: Vector[Question]
)

sealed trait Answer {
  def author: Option[User]
  def timeDate: LocalDateTime
}

sealed trait Question {
  type A <: Answer

  def id: Int
  def label: String
  def answers: Vector[A]
}

object Question {
  def assignId(q: Question, newId: Int) = q match {
    case x: OpenQuestion  => x.copy(id = newId)
    case x: CloseQuestion => x.copy(id = newId)
    case x: MultiQuestion => x.copy(id = newId)
  }
}

case class OpenQuestion(
    override val id: Int,
    override val label: String,
    override val answers: Vector[OpenAnswer]
) extends Question {
  type A = OpenAnswer
}

object OpenQuestion {
  def apply(label: String): OpenQuestion =
    new OpenQuestion(-1, label, Vector.empty)
}

case class OpenAnswer(
    override val author: Option[User],
    override val timeDate: LocalDateTime,
    value: String
) extends Answer

case class CloseQuestion(
    override val id: Int,
    override val label: String,
    options: List[String],
    override val answers: Vector[CloseAnswer]
) extends Question {
  type A = CloseAnswer
}

object CloseQuestion {
  def apply(label: String, options: List[String]): CloseQuestion =
    new CloseQuestion(-1, label, options, Vector.empty)
}

case class CloseAnswer(
    override val author: Option[User],
    override val timeDate: LocalDateTime,
    value: Int
) extends Answer

case class MultiQuestion(
    override val id: Int,
    override val label: String,
    options: List[String],
    override val answers: Vector[MultiAnswer]
) extends Question {
  type A = MultiAnswer
}

object MultiQuestion {
  def apply(label: String, options: List[String]): MultiQuestion =
    new MultiQuestion(-1, label, options, Vector.empty)
}

case class MultiAnswer(
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

  def votingLens(userId: String, id: Int): LensT[Try, VoteState, Voting] =
    lensT(
      s => Try(s.votings(id)),
      (s, v) =>
        Try {
          assert(v.owner.name == userId)
          s.copy(votings = s.votings + (id -> v.copy(id = id)))
      }
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
    (o, v) =>
      Try {
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

  def addQuestionLens(timeDate: LocalDateTime) =
    lensT[Try, Voting, Question](
      o => Try(o.questions.last),
      (o, v) =>
        Try {
          assertNotFreezed(o, timeDate)
          o.copy(
            questions = o.questions :+ Question
              .assignId(v, o.questions.map(_.id).max + 1)) // assign new is as max from all + 1
      }
    )

  def deleteQuestionLens(qId: Int, timeDate: LocalDateTime) =
    lens0[Try, Voting, Unit](
      o => (),
      (o, _) =>
        Try {
          assertNotFreezed(o, timeDate)
          o.copy(questions = o.questions.filterNot(_.id == qId))
      }
    )
}
