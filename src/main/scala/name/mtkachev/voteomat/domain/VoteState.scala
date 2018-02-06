package name.mtkachev.voteomat.domain

trait VoteState
trait Response

//state monad?
object VoteState {
  def change(state: VoteState, cmd: Command): (VoteState, Response) = ???
}