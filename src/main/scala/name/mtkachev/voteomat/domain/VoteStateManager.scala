package name.mtkachev.voteomat.domain

import java.time.LocalDateTime

import name.mtkachev.voteomat.domain.VoteStateManager.withChosenVotingId

import scala.util.{Failure, Try}

sealed trait ApplyRes
case class VotingCreated(id: Int) extends ApplyRes
case class VotingList(votings: List[Voting]) extends ApplyRes
case class VotingDeleted(id: Int) extends ApplyRes
case class VotingStarted(id: Int) extends ApplyRes
case class VotingStopped(id: Int) extends ApplyRes
case class VotingResult(voting: Voting) extends ApplyRes
case class View(voting: Voting) extends ApplyRes
case class QuestionAdded(maxdx: Int) extends ApplyRes
case class QuestionDeleted() extends ApplyRes
case class Voted() extends ApplyRes
case class CtxChanged() extends ApplyRes

object VoteStateManager {
  import cats.syntax.either._
  import cats.instances.try_._

  import name.mtkachev.voteomat.util.LensT._

  def apply(state: VoteState,
            time: LocalDateTime,
            ctx: CommonCommandContext,
            cmd: CommonCommand): Try[(VoteState, ApplyRes)] = {

    def addQuestion(q: Question) = {
      withChosenVotingId(ctx) { id =>
        val votingLens = VoteState.votingLens(ctx.userId, id)
        val addQuestionLens = Voting.addQuestionLens(time)
        compose(votingLens, addQuestionLens)
          .set(state, q)
          .flatMap(s =>
            votingLens.get(s).map { v =>
              (state, QuestionAdded(v.questions.size - 1))
          })
      }
    }

    cmd match {
      case cmd: CreateVoting =>
        val id = state.votings.size + 1
        VoteState
          .addVoting(
            state,
            Voting(
              id,
              User(ctx.userId),
              cmd.name,
              isAnonymous = cmd.anon,
              resultsAfterStop = cmd.viewAfterStop,
              isManualStarted = false,
              startDate = cmd.startDate,
              stopDate = cmd.stopDate,
              questions = Vector.empty
            )
          )
          .map((_, VotingCreated(id)))

      case cmd: ListVotings =>
        Try(state, VotingList(state.votings.values.toList))

      case DeleteVoting(id) =>
        VoteState
          .deleteVoting(state, id, time)
          .map((_, VotingDeleted(id)))

      case StartVoting(id) =>
        votingManualStartLens(ctx.userId, time, id)
          .set(state, true)
          .map((_, VotingStarted(id)))

      case StopVoting(id) =>
        votingManualStartLens(ctx.userId, time, id)
          .set(state, true)
          .map((_, VotingStopped(id)))

      case ViewVotingResult(id) =>
        VoteState
          .votingLens(ctx.userId, id)
          .get(state)
          .flatMap { v =>
            Try {
              Voting.assertResultCanBeViewed(v, time)
              VotingResult(v)
            }
          }
          .map((state, _))

      case cmd: ViewVoting =>
        withChosenVotingId(ctx) { id =>
          VoteState
            .votingLens(ctx.userId, id)
            .get(state)
            .map(v => (state, View(v)))
        }

      case cmd: AddOpenQuestion =>
        addQuestion(OpenQuestion(cmd.question))

      case cmd: AddCloseQuestion =>
        addQuestion(CloseQuestion(cmd.question, cmd.options))

      case cmd: AddMultiQuestion =>
        addQuestion(MultiQuestion(cmd.question, cmd.options))

      case cmd: DeleteQuestion =>
        withChosenVotingId(ctx) { id =>
          votingDeleteQuestionLens(ctx.userId, time, id, cmd.questionId)
            .set(state, ())
            .map(_ => (state, QuestionDeleted()))
        }

      case cmd: Vote =>
        withChosenVotingId(ctx) { id =>
          VoteState.votingLens(ctx.userId, id).get(state).map { voting =>
            val userId = if (voting.isAnonymous) None else Some(ctx.userId)
            compose(
              VoteState.votingLens(voting.owner.user, id),
              compose(Voting.questionLens(time, cmd.questionId, true),
                      Question.acceptAnswerLens(userId,
                                                time,
                                                cmd.answer,
                                                cmd.toOpenAnswer,
                                                cmd.toCloseAnswer,
                                                cmd.toMultiAnswer))
            )
          }
        }.map(_ => (state, Voted()))
    }
  }

  def withChosenVotingId[T](ctx: CommonCommandContext)(
      f: Int => Try[T]): Try[T] = {
    ctx.votingId
      .map(f)
      .getOrElse(Failure(new IllegalStateException("no voting choosen")))
  }

  def votingManualStartLens(userId: String, time: LocalDateTime, id: Int) = {
    compose(VoteState.votingLens(userId, id), Voting.manualStartedLens(time))
  }

  def votingDeleteQuestionLens(userId: String,
                               time: LocalDateTime,
                               id: Int,
                               qId: Int) = {
    compose(VoteState.votingLens(userId, id),
            Voting.deleteQuestionLens(qId, time))
  }
}
