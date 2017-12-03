package brainfck

import java.io.{InputStream, OutputStream}

import brainfck.Brainfck.{IRNode, optimize}

trait BrainfckImpl {
  def run(program: Seq[IRNode])(in: InputStream, out: OutputStream): Unit

  def run(program: String)(in: InputStream, out: OutputStream): Unit = {
    run(Brainfck.optimize(Brainfck.parse(program)))(in, out)
  }
}
