package brainfck
import java.io.InputStream

import brainfck.common.{Context, Pipeline}
import fastparse.WhitespaceApi

import scala.io.Source


package object v2 {

  // Optimized AST; emitted by the parser.
  sealed trait ASTNode
  case class Jump(delta: Int) extends ASTNode
  case class Mutate(delta: Int) extends ASTNode
  case object Read extends ASTNode
  case object Write extends ASTNode
  case class Loop(inst: Seq[ASTNode]) extends ASTNode

  type Program = Seq[ASTNode]

  object Parser extends Pipeline[InputStream, Program] {

    val White = WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace(CharPred(c => !"<>+-,.[]".contains(c)).rep)
    }

    import White._
    import fastparse.noApi._

    val `<>` = P(CharsWhileIn("<>").!).map(s => Jump(s.count('>' == _) - s.count('<' == _)))
    val `+-` = P(CharsWhileIn("+-").!).map(s => Mutate(s.count('+' == _) - s.count('-' == _)))
    val `,` = P(",").map(_ => Read)
    val `.` = P(".").map(_ => Write)
    val `[*]` = P("[" ~ bf ~ "]").map(Loop.apply)
    val bf: P[Program] = P((`<>` | `+-` | `,` | `.` | `[*]`).rep)

    override def run(ctx: Context)(source: InputStream) = {

      val lines = Source.fromInputStream(source, "UTF-8").getLines()

      bf.parseIterator(lines) match {
        case Parsed.Success(program, _) =>
          program
        case Parsed.Failure(expected, failIndex, extra) =>
          ctx.reporter.fatal("Parsing error")
      }
    }
  }
}
