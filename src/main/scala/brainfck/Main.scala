package brainfck

import java.io.{ByteArrayInputStream, File, FileInputStream}
import java.nio.charset.StandardCharsets

import brainfck.common.{Context, Reporter}
import brainfck.v2.{BrainfckBytecode, BrainfckFunctional, BrainfckImperative, Parser}


object Main {

  def time[T](block: => T): T = {
    val ticks = System.currentTimeMillis()
    val result = block
    println("Elapsed: " + (System.currentTimeMillis() - ticks) + "ms")
    result
  }

  def processOptions(args: Array[String]): Context = {
    val reporter = new Reporter()
    var files: List[File] = Nil
    var outDir: Option[File] = None

    def rec(args: List[String]): Unit = args match {
      case "-d" :: dir :: xs =>
        outDir = Some(new File(dir))
        rec(xs)

      case f :: xs =>
        files ::= new File(f)
        rec(xs)

      case _ =>
    }

    rec(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, " + files.size + " file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir)
  }

  def main(args: Array[String]): Unit = {
    val ctx =  Context(reporter = new Reporter(), file = null, outDir = null) //processOptions(args)

    val pipeline = Parser andThen BrainfckBytecode

    val source =
    """>>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[
      |->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<
      |]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>
      |+<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-
      |[<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[
      |>->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]""".stripMargin

    val sourceStream = new ByteArrayInputStream(source.getBytes(StandardCharsets.UTF_8.name()))

    val input =
      """
      """.stripMargin

    val inputStream = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8.name()))

    time {
      pipeline.run(ctx)(sourceStream)(inputStream, System.out)
    }
  }

}
