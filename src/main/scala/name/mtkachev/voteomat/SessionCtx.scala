package name.mtkachev.voteomat

import name.mtkachev.voteomat.domain._

import scala.util.{Success, Try}

class SessionCtx {
  private val lock = new Object
  private var ctx = Option.empty[CommonCommandContext]

  def applyCmd(cmd: CtxCommand): Try[ApplyRes] = {
    lock.synchronized {
      cmd match {
        case User(user) => ctx = Some(CommonCommandContext(user, None))
        case Begin(id)  => ctx = ctx.map(x => x.copy(votingId = Some(id)))
        case End()      => ctx = ctx.map(x => x.copy(votingId = None))
      }
      Success(CtxChanged())
    }
  }

  def isEmpty: Boolean = lock.synchronized {
    ctx.isEmpty
  }

  def get: CommonCommandContext = lock.synchronized {
    ctx.get
  }
}
