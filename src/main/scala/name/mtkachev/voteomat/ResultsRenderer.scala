package name.mtkachev.voteomat

import name.mtkachev.voteomat.domain.ApplyRes

import scala.util.{Failure, Success, Try}

trait ResultsRenderer {
  def render(res: Try[ApplyRes]): String
}

class ResultsRendererImpl extends ResultsRenderer {
  override def render(res: Try[ApplyRes]): String = res match {
    case Failure(t) => s"ERROR: ${t.getMessage}"
    case Success(res) => res.toString
  }
}
