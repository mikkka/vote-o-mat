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

object VoteStateManager {
  import cats.syntax.either._
  import cats.instances.try_._

  import name.mtkachev.voteomat.util.LensT._

  def apply(state: VoteState,
            time: LocalDateTime,
            ctx: CommonCommandContext,
            cmd: CommonCommand): Either[Throwable, (VoteState, ApplyRes)] = {
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
              questions = List.empty
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
        votingManualStartLens(time, id)
          .set(state, true)
          .map((_, VotingStarted(id)))
          .toEither

      case StopVoting(id) =>
        votingManualStartLens(time, id)
          .set(state, true)
          .map((_, VotingStopped(id)))
          .toEither

      case ViewVotingResult(id) =>
        VoteState
          .votingLens(id)
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
        ctx.votingId
          .map { id =>
            VoteState.votingLens(id).get(state).map(v => (state, View(v)))
          }
          .getOrElse(Failure(new IllegalStateException("no voting choosen")))
          .toEither

      case cmd: AddOpenQuestion  => ???
      case cmd: AddCloseQuestion => ???
      case cmd: AddMultiQuestion => ???
      case cmd: DeleteQuestion   => ???
      case cmd: Vote             => ???
    }
  }

  def votingManualStartLens(time: LocalDateTime, id: Int) = {
    compose(VoteState.votingLens(id), Voting.manualStartedLens(time))
  }
}
