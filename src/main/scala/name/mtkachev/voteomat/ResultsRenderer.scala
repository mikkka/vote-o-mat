package name.mtkachev.voteomat

import java.time.LocalDateTime

import cats.Show
import name.mtkachev.voteomat.domain._

import scala.util.{Failure, Success, Try}
import cats.syntax.show._

import scala.reflect.ClassTag
import scala.reflect.api._

trait ResultsRenderer {
  def render(res: Try[ApplyRes]): String
}

class ResultsRendererImpl extends ResultsRenderer {

  import ElemsShow0._

  override def render(res: Try[ApplyRes]): String = res match {
    case Failure(t) => s"ERROR: ${t.getMessage}"
    case Success(res) => res match {
      case VotingCreated(id: Int) =>
        s"опрос создан: $id"

      case VotingList(votings: List[Voting]) =>
        votings.map(_.show).mkString("\n")

      case VotingDeleted(id: Int) =>
        s"опрос удален: $id"

      case VotingStarted(id: Int) =>
        s"опрос начат: $id"

      case VotingStopped(id: Int) =>
        s"опрос окончен: $id"

      case View(voting: Voting) =>
        voting.show + "\n" +
          voting.questions.map (_.show)
          .mkString("\n")

      case VotingResult(voting: Voting) =>
        import ElemShow1._

        voting.show + "\n" +
          voting.questions.map (_.show)
            .mkString("\n")

      case QuestionAdded(maxdx: Int) =>
        s"вопрос создан: $maxdx"

      case QuestionDeleted() =>
        s"вопрос удален"

      case Voted() =>
        s"ответ отправлен"

      case CtxChanged() =>
        s"контекст работы изменен"
    }
  }
}

trait ElemsShow0 {
  implicit def dateShow: Show[LocalDateTime] = Show.show(x => x.toString)

  implicit def votingShow: Show[Voting] = Show.show { v =>
    s"""#${v.id} ${v.name} ${if (v.isManualStarted) "[активен]" else ""} ${v.startDate.map(_.show).getOrElse("")} ${v.stopDate.map(_.show).getOrElse("")} ${if (v.isAnonymous) "[анонимный]" else ""} ${if (v.resultsAfterStop) "[результаты после]" else ""}"""
  }

  implicit def questionShow: Show[Question] = Show.show {
    case x: OpenQuestion => x.show
    case x: CloseQuestion => x.show
    case x: MultiQuestion => x.show
  }

  implicit def openQuestionShow: Show[OpenQuestion] = Show.show { q =>
    s"[#${q.id}] ${q.label}"
  }

  implicit def closeQuestionShow: Show[CloseQuestion] = Show.show { q =>
    s"[#${q.id}] ${q.label} [close]: " +
      q.options.zipWithIndex.map { case (o, idx) => s"#$idx $o" }.mkString("\n")
  }

  implicit def multiQuestionShow: Show[MultiQuestion] = Show.show { q =>
    s"[#${q.id}] ${q.label} [multi]: " +
      q.options.zipWithIndex.map { case (o, idx) => s"#$idx $o" }.mkString("\n")
  }
}

object ElemsShow0 extends ElemsShow0

trait ElemsShow1 extends ElemsShow0 {
  implicit def openAnswerShow: Show[OpenAnswer] = Show.show { a =>
    a.author.map(au => s"${au.user} : ").getOrElse("") + a.value
  }

  implicit def openQuestionShowWithAnswers: Show[OpenQuestion] = Show.show { q =>
    s"[#${q.id}] ${q.label}" +
      (if (q.answers.nonEmpty) "\n" else "") +
      q.answers.map(_.show).mkString("\n")
  }

  implicit def closeQuestionShowWithAnswers: Show[CloseQuestion] = Show.show { q =>
    val ansMap = q.answers.map(_.value).groupBy(identity).map(kv => kv._1 -> kv._2.size)

    s"[#${q.id}] ${q.label} [close]: " +
      q.options.zipWithIndex.map { case (o, idx) => s"#$idx $o : ${ansMap.getOrElse(idx, 0)}" }.mkString("\n")
  }

  implicit def multiQuestionShowWithAnswers: Show[MultiQuestion] = Show.show { q =>
    val ansMap = q.answers.flatMap(_.value).groupBy(identity).map(kv => kv._1 -> kv._2.size)

    s"[#${q.id}] ${q.label} [multi]: " +
      q.options.zipWithIndex.map { case (o, idx) => s"#$idx $o : ${ansMap.getOrElse(idx, 0)}" }.mkString("\n")
  }
}

object ElemShow1 extends ElemsShow1