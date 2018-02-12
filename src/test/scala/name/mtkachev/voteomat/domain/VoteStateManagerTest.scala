package name.mtkachev.voteomat.domain

import java.time.LocalDateTime

import org.scalatest.FlatSpec
import org.scalatest._
import Matchers._

import scala.util.Try

class VoteStateManagerTest extends FlatSpec {
  "VoteState" should "evolve in scenario 1" in {
    val ctx = CommonCommandContext("userFoo", None)
    val ctxVoter = CommonCommandContext("userBar", None)
    var state = VoteState(Map.empty)

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctx,
             CreateVoting(
               "vote1",
               false,
               true,
               None,
               None
             ))
      .get
      ._1

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctx.copy(votingId = Some(1)),
             AddOpenQuestion("q1"))
      .get
      ._1

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctx.copy(votingId = Some(1)),
             AddCloseQuestion("q2", List("a1", "a2", "a3")))
      .get
      ._1

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctx.copy(votingId = Some(1)),
             AddMultiQuestion("q3", List("a1", "a2", "a3")))
      .get
      ._1

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctx.copy(votingId = Some(1)),
             AddMultiQuestion("q4", List("a1", "a2", "a3")))
      .get
      ._1

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctx.copy(votingId = Some(1)),
             DeleteQuestion(3))
      .get
      ._1

    state.votings(1).questions.size should be(3)

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctx.copy(votingId = None),
             StartVoting(1))
      .get
      ._1

    state.votings(1).isManualStarted should be(true)

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctx.copy(votingId = None),
             StopVoting(1))
      .get
      ._1

    state.votings(1).isManualStarted should be(false)

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctx.copy(votingId = None),
             StartVoting(1))
      .get
      ._1

    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctxVoter.copy(votingId = Some(1)),
             Vote(0, "kek", x => Try(x), null, null))
      .get
      ._1
    state = VoteStateManager
      .apply(state,
             LocalDateTime.now(),
             ctxVoter.copy(votingId = Some(1)),
             Vote(0, "lul", x => Try(x), null, null))
      .get
      ._1

    state.votings(1).questions(0).answers.size should be(2)

    state = VoteStateManager
      .apply(state,
        LocalDateTime.now(),
        ctxVoter.copy(votingId = Some(1)),
        Vote(1, "kek", null, x => Try(0), null))
      .get
      ._1
    state = VoteStateManager
      .apply(state,
        LocalDateTime.now(),
        ctxVoter.copy(votingId = Some(1)),
        Vote(1, "lul", null, x => Try(1), null))
      .get
      ._1

    state.votings(1).questions(1).answers.size should be(2)

    state = VoteStateManager
      .apply(state,
        LocalDateTime.now(),
        ctxVoter.copy(votingId = Some(1)),
        Vote(2, "kek", null, null, x => Try(Set(0, 1))))
      .get
      ._1
    state = VoteStateManager
      .apply(state,
        LocalDateTime.now(),
        ctxVoter.copy(votingId = Some(1)),
        Vote(2, "lul", null, null, x => Try(Set(2, 1))))
      .get
      ._1

    state.votings(1).questions(2).answers.size should be(2)
  }
}
