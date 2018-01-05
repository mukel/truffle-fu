package brainfck.common

import java.io.{InputStream, OutputStream}


trait BrainfckImpl[P] extends Pipeline[P, (InputStream, OutputStream) => Unit] {

  def run(program: P)(in: InputStream, out: OutputStream): Unit

  override def run(ctx: Context)(program: P) = {
    // preprocess
    // ...
    // execute
    run(program)
  }
}
