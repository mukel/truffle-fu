package brainfck

object Main {

  def time[T](block: => T): T = {
    val ticks = System.currentTimeMillis()
    val result = block
    println("Elapsed: " + (System.currentTimeMillis() - ticks) + "ms")
    result
  }

  def main(args: Array[String]): Unit = {

    val bfInterpreter =
    """>>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[
      |->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<
      |]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>
      |+<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-
      |[<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[
      |>->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]""".stripMargin

    //val in = new ByteArrayInputStream(input).getBytes(StandardCharsets.UTF_8.name()))
    time {
      new BrainfckBytecode().run(bfInterpreter)(System.in, System.out)
    }
  }
}
