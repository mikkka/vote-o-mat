package name.mtkachev.voteomat.domain

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

import cats.syntax.either._

trait CommandParser {
  def parse(txt: String): Either[String, Command]
}

trait SimpleCommandParser extends CommandParser with RegexParsers {
  //eliminate white space skipping
  override val whiteSpace = "".r

  override def parse(txt: String): Either[String, Command] = {
    val parseResult = cmd.apply(new CharSequenceReader(txt))
    if (parseResult.successful) parseResult.get.asRight
    else "parse error".asLeft
  }

  def cmd: Parser[Command] =
    cmdCreateVoting |
      cmdListVotings |
      cmdDeleteVoting |
      cmdStartVoting |
      cmdStopVoting |
      cmdViewVotingResult |
      cmdViewVoting |
      cmdAddQuestion |
      cmdDeleteQuestion |
      cmdVote |
      cmdBegin |
      cmdEnd

  private val anonConst = Map("yes" -> true, "no" -> false)
  private val viewConst = Map("afterstop" -> true, "continuous" -> false)

  def cmdCreateVoting: Parser[CreateVoting] =
    "/create_vote" ~
      spaces ~
      stringArg ~
      opt(
        spaces ~
        stringArg ~ // anon
          opt(
            spaces ~
            stringArg ~ // view
              opt(
                spaces ~
                dateArg ~ // start
                  opt(
                    spaces ~
                    dateArg // end
                  )
             )
          )
      ) ^^
      {
        case _ ~ _ ~ question ~ None => CreateVoting(question, anon = true, viewAfterStop = true, None, None)

        case _ ~ _ ~ question ~ Some(_ ~ anon ~ None) if anonConst.isDefinedAt(anon) =>
          CreateVoting(question, anonConst(anon), viewAfterStop = true, None, None)

        case _ ~ _ ~ question ~ Some(_ ~ anon ~ Some(_ ~ view ~ None))
          if anonConst.isDefinedAt(anon) && viewConst.isDefinedAt(view )=>
          CreateVoting(question, anonConst(anon), viewConst(view), None, None)

        case _ ~ _ ~ question ~ Some(_ ~ anon ~ Some(_ ~ view ~ Some(_ ~ start ~ None)))
          if anonConst.isDefinedAt(anon) && viewConst.isDefinedAt(view )=>
          CreateVoting(question, anonConst(anon), viewConst(view), Some(start), None)

        case _ ~ _ ~ question ~ Some(_ ~ anon ~ Some(_ ~ view ~ Some(_ ~ start ~ Some(_ ~ end))))
          if anonConst.isDefinedAt(anon) && viewConst.isDefinedAt(view )=>
          CreateVoting(question, anonConst(anon), viewConst(view), Some(start), Some(end))
      }


  def cmdListVotings: Parser[ListVotings] = "/list" ^^ (_ => ListVotings())

  def cmdDeleteVoting: Parser[DeleteVoting] =
    "/delete_vote" ~
      spaces ~
      numArg ^^ {case _ ~ _ ~ votingId => DeleteVoting(votingId)}

  def cmdStartVoting: Parser[StartVoting] =
    "/start_vote" ~
      spaces ~
      numArg ^^ {case _ ~ _ ~ votingId => StartVoting(votingId)}

  def cmdStopVoting: Parser[StopVoting] =
    "/stop_vote" ~
      spaces ~
      numArg ^^ {case _ ~ _ ~ votingId => StopVoting(votingId)}

  def cmdViewVotingResult: Parser[ViewVotingResult] =
    "/result" ~
      spaces ~
      numArg ^^ {case _ ~ _ ~ votingId => ViewVotingResult(votingId)}

  def cmdViewVoting: Parser[ViewVoting] = "/view" ^^ (_ => ViewVoting())

  def cmdAddQuestion: Parser[AddQuestion] =
    "/create_question" ~
      spaces ~
      stringArg ~
      opt(spaces ~ stringArg) ~ opt(spaces ~ repsep(stringArg, spaces)) ^?
      {
        case _ ~ _ ~ question ~ Some(_ ~ qType) ~ Some(_ ~ opts)
          if qType == "choice" && opts.nonEmpty =>
          AddCloseQuestion(question, opts)
        case _ ~ _ ~ question ~ Some(_ ~ qType) ~ Some(_ ~ opts)
          if qType == "multi" && opts.nonEmpty =>
          AddMultiQuestion(question, opts)
        case _ ~ _ ~ question ~ Some(_ ~ qType) ~ None if qType == "open" =>
          AddOpenQuestion(question)
        case _ ~ _ ~ question ~ None ~ None =>
          AddOpenQuestion(question)
      }

  def cmdDeleteQuestion: Parser[DeleteQuestion] =
    "/delete_question" ~
      spaces ~
      numArg ^^ {case _ ~ _ ~ qId => DeleteQuestion(qId )}

  def cmdVote: Parser[Vote] =
    "/vote" ~
      spaces ~
      numArg ~
      spaces ~
      stringArg ^^ { case _ ~ _ ~ qId ~ _ ~ ans => Vote(qId, ans) }

  def cmdBegin: Parser[Begin] =
    "/begin" ~
      spaces ~
      numArg ^^ {case _ ~ _ ~ votingId => Begin(votingId)}

  def cmdEnd: Parser[End] = "/end" ^^ (_ => End())


  def dateArg: Parser[LocalDateTime] = "(" ~ date ~ ")" ^^ {case _ ~ b ~ _ => b}

  def numArg: Parser[Int] = "(" ~ number ~ ")" ^^ {case _ ~ b ~ _ => b}

  def stringArg: Parser[String] = "(" ~ string ~ ")" ^^ {case _ ~ b ~ _ => b}

  def date: Parser[LocalDateTime] = """\d\d:\d\d:\d\d \d\d:\d\d:\d\d""".r ^^
    (LocalDateTime.parse(_, DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd")))

  def number: Parser[Int] = """\d+(\.\d*)?""".r ^^ { _.toInt }

  def string: Parser[String] = rep(validStrLetter) ^^ {xs => xs.mkString}

  def validStrLetter: Parser[String] =
    lftBr |
    rgtBr |
    """[^()]""".r ^^
//    """([^"\x00-\x1F\x7F\\(\\)\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})""".r  ^^
      {_.replace("((", "(").replace("))", ")")}

  def lftBr: Parser[String] = "((" ^^ (_ => "(")

  def rgtBr: Parser[String] = "))" ^^ (_ => ")")

  def spaces: Parser[String] = """\s+""".r
}
