package name.mtkachev.voteomat

import java.time.LocalDateTime

import name.mtkachev.voteomat.domain._

import scala.util.Try

class VoteStateCtx {
  private val lock = new Object
  private var state = new VoteState(Map.empty)

  def applyCmd(ctx: CommonCommandContext, cmd: CommonCommand): Try[ApplyRes] = {
    val time = LocalDateTime.now()
    lock.synchronized {
      val res = VoteStateManager.apply(state, time, ctx, cmd)
      res.foreach(x => state = x._1)
      res.map(_._2)
    }
  }
}
