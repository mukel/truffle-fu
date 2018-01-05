package brainfck.common

abstract class Pipeline[-I, +O] {
  self =>

  def andThen[T](thenn: Pipeline[O, T]): Pipeline[I, T] = new Pipeline[I, T] {
    def run(ctx: Context)(v: I): T = {
      val first = self.run(ctx)(v)
      ctx.reporter.terminateIfErrors
      thenn.run(ctx)(first)
    }
  }

  def run(ctx: Context)(v: I): O
}


import java.io.File

case class Context(
                    reporter: Reporter,
                    file: File,
                    outDir: Option[File] = None
                  )


import java.io.File

import scala.io.Source

class Reporter {

  var hasErrors = false
  var filesToLines = Map[File, IndexedSeq[String]]()

  def info(msg: Any, pos: Positioned = NoPosition): Unit = {
    report("Info", msg, pos)
  }

  private def report(prefix: String, msg: Any, pos: Positioned) {
    if (pos.hasPosition) {
      err(pos.position + ": " + prefix + ": " + msg.toString)

      val lines = getLines(pos.file)

      if (pos.line - 1 < lines.size) {
        err(lines(pos.line - 1))
        err(" " * pos.col + "^")
      } else {
        err("<line unavailable in source file>")
      }
    } else {
      err(prefix + ": " + msg.toString)
    }
  }

  private def err(msg: String) {
    System.err.println(msg)
  }

  private def getLines(f: File): IndexedSeq[String] = {
    filesToLines.get(f) match {
      case Some(lines) =>
        lines

      case None =>
        val source = Source.fromFile(f).withPositioning(true)
        val lines = source.getLines().toIndexedSeq
        source.close()

        filesToLines += f -> lines

        lines
    }
  }

  def warning(msg: Any, pos: Positioned = NoPosition): Unit = {
    report("Warning", msg, pos)
  }

  def error(msg: Any, pos: Positioned = NoPosition): Unit = {
    hasErrors = true
    report("Error", msg, pos)
  }

  def fatal(msg: Any, pos: Positioned = NoPosition): Nothing = {
    report("Fatal", msg, pos)
    sys.exit(1);
  }

  def terminateIfErrors = {
    if (hasErrors) {
      err("There were errors.")
      sys.exit(1);
    }
  }
}


import java.io.File

trait Positioned {
  /** Number of bits used to encode the line number */
  final private[this] val LINE_BITS = 20
  /** Number of bits used to encode the column number */
  final private[this] val COLUMN_BITS = 31 - LINE_BITS // no negatives => 31
  /** Mask to decode the line number */
  final private[this] val LINE_MASK = (1 << LINE_BITS) - 1
  /** Mask to decode the column number */
  final private[this] val COLUMN_MASK = (1 << COLUMN_BITS) - 1
  private[Positioned] var _file: Option[File] = None
  private[Positioned] var _line: Int = 0
  private[Positioned] var _col: Int = 0

  def setPos(file: File, pos: Int): this.type = {

    _line = lineOf(pos)
    _col = columnOf(pos)
    _file = Some(file)

    this
  }

  private[this] def lineOf(pos: Int): Int = (pos >> COLUMN_BITS) & LINE_MASK

  private[this] def columnOf(pos: Int): Int = pos & COLUMN_MASK

  def setPos(other: Positioned): this.type = {
    _line = other._line
    _col = other._col
    _file = other._file

    this
  }

  def position: String = {
    if (hasPosition) {
      file.getPath + ":" + line + ":" + col
    } else {
      "?:?"
    }
  }

  def hasPosition = _file.isDefined

  def file = _file.get

  def line = _line

  def col = _col
}

case object NoPosition extends Positioned