package name.mtkachev.voteomat.domain

import java.time.LocalDateTime

import org.scalatest.FlatSpec
import org.scalatest._
import Matchers._

import cats.syntax.either._
import scala.util.parsing.input.CharSequenceReader

class SimpleCommandParserTest extends FlatSpec with SimpleCommandParser {
  "SimpleCommandParser" should "parse string literal" in {
    val res = string(new CharSequenceReader("""cvno;awk;vwd sxcweo  ': x, cqqjdfc pwefjw (()) (())"""))
    res.successful should be(true)
    res.get should be("""cvno;awk;vwd sxcweo  ': x, cqqjdfc pwefjw () ()""")
  }

  it should "parse string arg" in {
    val res = stringArg(new CharSequenceReader("((())*&^%((asd asd 9191221)))"))
    res.successful should be(true)
    res.get should be("()*&^%(asd asd 9191221)")
  }

  it should "parse num arg" in {
    val res = numArg(new CharSequenceReader("(09191221)"))
    res.successful should be(true)
    res.get should be(9191221)
  }

  it should "parse date arg" in {
    val res = dateArg(new CharSequenceReader("(12:01:21 19:02:20)"))
    res.successful should be(true)
    res.get should be(LocalDateTime.of(2019, 2, 20, 12, 1, 21))
  }

  it should "parse cmdCreateVoting" in {
    parse("/create_vote (кто здесь) (no) (continuous) (10:10:02 19:02:20) (14:01:21 20:02:20)") should
      be(Right(
        CreateVoting("кто здесь", false, false,
          Some(LocalDateTime.of(2019, 2, 20, 10, 10, 2)),
          Some(LocalDateTime.of(2020, 2, 20, 14, 1, 21)))))

    parse("/create_vote (кто здесь?) (no) (continuous) (10:10:02 19:02:20)") should
      be(Right(
        CreateVoting("кто здесь?", false, false,
          Some(LocalDateTime.of(2019, 2, 20, 10, 10, 2)), None)))

    parse("/create_vote (кто здесь!) (no) (continuous)") should
      be(Right(CreateVoting("кто здесь!", false, false, None, None)))

    parse("/create_vote (кто здесь#) (no)") should
      be(Right(CreateVoting("кто здесь#", false, true, None, None)))

    parse("/create_vote (кто здесь - 012)") should
      be(Right(CreateVoting("кто здесь - 012", true, true, None, None)))
  }

  it should "parse cmdListVotings" in {
    parse("/list") should be(Right(ListVotings()))
  }

  it should "parse cmdDeleteVoting" in {
    parse("/delete_vote (123)") should be(Right(DeleteVoting(123)))
  }

  it should "parse cmdStartVoting" in {
    parse("/start_vote (12323)") should be(Right(StartVoting(12323)))
  }

  it should "parse cmdStopVoting" in {
    parse("/stop_vote (4564)") should be(Right(StopVoting(4564)))
  }

  it should "parse cmdViewVotingResult" in {
    parse("/result (99234)") should be(Right(ViewVotingResult(99234)))
  }

  it should "parse cmdViewVoting" in {
    parse("/view") should be(Right(ViewVoting()))
  }

  it should "parse cmdAddQuestion" in {
    parse("/create_question (what is up?)") should be(AddOpenQuestion("what is up?").asRight)
    parse("/create_question (what is up?) (open)") should be(AddOpenQuestion("what is up?").asRight)
    parse(
      """/create_question (what is up?) (choice)
        |(asd asd asd1)
        |(asd asd klaslkasklas2)""".stripMargin) should be(
      AddCloseQuestion("what is up?", List("asd asd asd1", "asd asd klaslkasklas2")).asRight)

    parse(
      """/create_question (what is up?) (multi)
        |(asd asd asd1)
        |(asd asd klaslkasklas2)""".stripMargin) should be(
      AddMultiQuestion("what is up?", List("asd asd asd1", "asd asd klaslkasklas2")).asRight)
  }

  it should "parse cmdDeleteQuestion" in {
    parse("/delete_question (349)") should be(Right(DeleteQuestion(349)))
  }

  it should "parse cmdVote" in {
    parse("/vote (09) (ответы ответы ответы ответы)") should be(Right(Vote(9, "ответы ответы ответы ответы")))
  }

  it should "parse cmdBegin" in {
    parse("/begin (22)") should be(Right(Begin(22)))
  }

  it should "parse cmdEnd" in {
    parse("/end") should be(Right(End()))
  }
}
