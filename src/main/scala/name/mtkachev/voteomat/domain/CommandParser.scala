package name.mtkachev.voteomat.domain

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

trait CommandParser {
  def parse(txt: String, context: CommandContext): Either[String, Command]
}

trait SimpleCommandParser extends CommandParser with RegexParsers {
  def cmd: Parser[Command] =
    cmdCreateVoting |
      cmdListVotings |
      cmdDeleteVoting |
      cmdStartVoting |
      cmdStopVoting |
      cmdViewVotingResult |
      cmdAddOpenQuestion |
      cmdAddCloseQuestion |
      cmdAddCloseQuestion |
      cmdAddMultiQuestion |
      cmdDeleteQuestion |
      cmdVote
  
  def cmdCreateVoting: Parser[CreateVoting]
  def cmdListVotings: Parser[ListVotings]
  def cmdDeleteVoting: Parser[DeleteVoting]
  def cmdStartVoting: Parser[StartVoting]
  def cmdStopVoting: Parser[StopVoting]
  def cmdViewVotingResult: Parser[ViewVotingResult]
  def cmdAddOpenQuestion: Parser[AddOpenQuestion]
  def cmdAddCloseQuestion: Parser[AddCloseQuestion]
  def cmdAddMultiQuestion: Parser[AddMultiQuestion]
  def cmdDeleteQuestion: Parser[DeleteQuestion]
  def cmdVote: Parser[Vote]

  def arg: Parser[String] = "("

  def number: Parser[Int] = """\d+(\.\d*)?""".r ^^ { _.toInt }
  def string: Parser[String] =
    lftBr |
    rgtBr |
    """([^"\x00-\x1F\x7F\\(\\)\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})""".r

  def lftBr: Parser[String] = "((" ^^ (_ => "(")
  def rgtBr: Parser[String] = "))" ^^ (_ => ")")
}
