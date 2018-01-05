package brainfck.v2

import java.io.{InputStream, OutputStream}

import brainfck.common.BrainfckImpl

object BrainfckImperative extends BrainfckImpl[Program] {

  def run(program: Seq[ASTNode])(in: InputStream, out: OutputStream): Unit = {
    var ptr = 0
    val tape = Array.fill(1 << 16)(0)
    def execute(op: ASTNode): Unit = {
      op match {
        case Jump(delta) => ptr += delta
        case Mutate(delta) => tape(ptr) += delta
        case Read => tape(ptr) = in.read()
        case Write =>
          out.write(tape(ptr))
          out.flush() // flush for debugging
        case Loop(body) =>
          while (tape(ptr) != 0)
            body.foreach(execute)
      }
    }
    program.foreach(execute)
  }
}
