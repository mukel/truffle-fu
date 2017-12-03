package brainfck

import java.io.{InputStream, OutputStream}

import brainfck.Brainfck._

class BrainfckImperative extends BrainfckImpl {
  def run(program: Seq[IRNode])(in: InputStream, out: OutputStream): Unit = {
    var ptr = 0
    val tape = Array.fill(1 << 16)(0)
    def execute(op: IRNode): Unit = {
      op match {
        case Jump(delta) => ptr += delta
        case Mutate(delta) => tape(ptr) += delta
        //case BoomerangMutate(deltaPtr, deltaValue) => tape(ptr + deltaPtr) += deltaValue
        case Read => tape(ptr) = in.read()
        case Write =>
          out.write(tape(ptr))
          out.flush() // flush for debugging
        case Loop(body) =>
          while (tape(ptr) != 0)
            body.foreach(execute)
        //case Assign(value) => tape(ptr) = value
      }
    }
    program.foreach(execute)
  }
}
