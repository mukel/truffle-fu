package brainfck.v2

import java.io.{InputStream, OutputStream}

import brainfck.common.BrainfckImpl

/**
  * Functional interpreter, the program state is immutable.
  */
object BrainfckFunctional extends BrainfckImpl[Program] {

  case class State(ptr: Int, tape: Vector[Int])

  def run(program: Program)(in: InputStream, out: OutputStream): Unit = {
    def execute(state: State, op: ASTNode): State = {
      val State(ptr, tape) = state
      op match {
        case Jump(delta) => State(ptr + delta, tape)
        case Mutate(delta) => State(ptr, tape.updated(ptr, tape(ptr) + delta))
        case Read => State(ptr, tape.updated(ptr, in.read()))
        case Write =>
          out.write(tape(ptr))
          out.flush() // flush for debugging
          state
        case Loop(body) =>
          Iterator.iterate(state)(body.foldLeft(_)(execute))
            .find(s => s.tape(s.ptr) == 0)
            .get
      }
    }
    program.foldLeft(State(0, Vector.fill(1 << 16)(0)))(execute)
  }
}
