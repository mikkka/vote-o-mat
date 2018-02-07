package name.mtkachev.voteomat.domain

import java.time.LocalDateTime

import scala.util.{Failure, Try}

sealed trait ApplyRes
case class VotingCreated(id: Int) extends ApplyRes
case class VotingList(votings: List[Voting]) extends ApplyRes
case class VotingDeleted(id: Int) extends ApplyRes
case class VotingStarted(id: Int) extends ApplyRes
case class VotingStopped(id: Int) extends ApplyRes
case class VotingResult(voting: Voting) extends ApplyRes
case class View(voting: Voting) extends ApplyRes
case class QuestionAdded() extends ApplyRes
case class QuestionDeleted() extends ApplyRes

object VoteStateManager {
  import cats.syntax.either._
  import cats.instances.try_._

  import name.mtkachev.voteomat.util.LensT._

  def apply(state: VoteState,
            time: LocalDateTime,
            ctx: CommonCommandContext,
            cmd: CommonCommand): Either[Throwable, (VoteState, ApplyRes)] = {

    def addQuestion(q: Question) = {
      withChoseVotingId(ctx) { id =>
        votingAddQuestionLens(ctx.userId, time, id)
          .set(state, q)
          .map(v => (state, QuestionAdded()))
      }.toEither
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
          .toEither

      case cmd: ListVotings =>
        (state, VotingList(state.votings.values.toList)).asRight

      case DeleteVoting(id) =>
        VoteState
          .deleteVoting(state, id, time)
          .map((_, VotingDeleted(id)))
          .toEither

      case StartVoting(id) =>
        votingManualStartLens(ctx.userId, time, id)
          .set(state, true)
          .map((_, VotingStarted(id)))
          .toEither

      case StopVoting(id) =>
        votingManualStartLens(ctx.userId, time, id)
          .set(state, true)
          .map((_, VotingStopped(id)))
          .toEither

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
          .toEither

      case cmd: ViewVoting =>
        withChoseVotingId(ctx) { id =>
          VoteState
            .votingLens(ctx.userId, id)
            .get(state)
            .map(v => (state, View(v)))
        }.toEither

      case cmd: AddOpenQuestion =>
        addQuestion(OpenQuestion(cmd.question))

      case cmd: AddCloseQuestion =>
        addQuestion(CloseQuestion(cmd.question, cmd.options))

      case cmd: AddMultiQuestion =>
        addQuestion(MultiQuestion(cmd.question, cmd.options))

      case cmd: DeleteQuestion   =>
        withChoseVotingId(ctx) { id =>
          votingDeleteQuestionLens(ctx.userId, time, id, cmd.questionId)
            .set(state, ()).map(_ => (state, QuestionDeleted()))
        }.toEither

      case cmd: Vote             => ???
    }
  }

  def withChoseVotingId[T](ctx: CommonCommandContext)(
      f: Int => Try[T]): Try[T] = {
    ctx.votingId
      .map(f)
      .getOrElse(Failure(new IllegalStateException("no voting choosen")))
  }

  def votingManualStartLens(userId: String, time: LocalDateTime, id: Int) = {
    compose(VoteState.votingLens(userId, id), Voting.manualStartedLens(time))
  }

  def votingAddQuestionLens(userId: String, time: LocalDateTime, id: Int) = {
    compose(VoteState.votingLens(userId, id), Voting.addQuestionLens(time))
  }

  def votingDeleteQuestionLens(userId: String,
                               time: LocalDateTime,
                               id: Int,
                               qId: Int) = {
    compose(VoteState.votingLens(userId, id),
            Voting.deleteQuestionLens(qId, time))
  }
}