package brainfck

import java.io.{InputStream, OutputStream}

import brainfck.Brainfck.{IRNode, _}

/**
  * Functional interpreter, the program state is immutable.
  */
class BrainfckFunctional extends BrainfckImpl {

  case class State(ptr: Int, tape: Vector[Int])

  def run(program: Seq[IRNode])(in: InputStream, out: OutputStream): Unit = {
    def execute(state: State, op: IRNode): State = {
      val State(ptr, tape) = state
      op match {
        case Jump(delta) => State(ptr + delta, tape)
        case Mutate(delta) => State(ptr, tape.updated(ptr, tape(ptr) + delta))
        //case BoomerangMutate(deltaPtr, deltaValue) =>
          //State(ptr, tape.updated(ptr + deltaPtr, tape(ptr + deltaPtr) + deltaValue))
        case Read => State(ptr, tape.updated(ptr, in.read()))
        case Write =>
          out.write(tape(ptr))
          out.flush() // flush for debugging
          state
        case Loop(body) =>
          Iterator.iterate(state)(body.foldLeft(_)(execute))
            .find(s => s.tape(s.ptr) == 0)
            .get
        case Assign(value) => State(ptr, tape.updated(ptr, value))
      }
    }
    program.foldLeft(State(0, Vector.fill(1 << 16)(0)))(execute)
  }
}
