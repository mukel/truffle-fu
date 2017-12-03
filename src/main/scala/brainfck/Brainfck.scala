package brainfck

import scala.annotation.tailrec

object Brainfck {

  // Optimized IR; emitted by the parser.
  sealed trait IRNode
  case class Jump(delta: Int) extends IRNode
  case class Mutate(delta: Int) extends IRNode
  case object Read extends IRNode
  case object Write extends IRNode
  case class Loop(inst: Seq[IRNode]) extends IRNode

  // Extra nodes produced by the optimizer

  // Assign(4) is equivalent to "[-]++++"
  // tape(ptr) += value
  case class Assign(value: Int) extends IRNode

  // BoomerangMutate(3, 2) is equivalent to ">>>++<<<"
  // tape(ptr + deltaPtr) += deltaValue
  case class BoomerangMutate(deltaPtr: Int, deltaValue: Int) extends IRNode

  import fastparse.all._

  val `<>` = P(CharsWhileIn("<>").!).map(s => Jump(s.count('>' == _) - s.count('<' == _)))
  val `+-` = P(CharsWhileIn("+-").!).map(s => Mutate(s.count('+' == _) - s.count('-' == _)))
  val `,` = P(",").map(_ => Read)
  val `.` = P(".").map(_ => Write)
  val `[*]` = P("[" ~ bf ~ "]").map(Loop.apply)
  val bf: P[Seq[IRNode]] = P((`<>` | `+-` | `,` | `.` | `[*]`).rep)

  def parse(source: String): Seq[IRNode] = {
    bf.parse(source.filter("<>+-.,[]".contains(_))) match {
      case Parsed.Success(nodes, _) =>
        nodes
      case f : Parsed.Failure =>
        println(f)
        System.exit(-1)
        ???
    }
  }

  def optimizerPass(program: List[IRNode]): List[IRNode] = {
    program match {
      // Drop effect-less nodes
      case Mutate(0) :: tail => optimizerPass(tail)
      case Jump(0) :: tail => optimizerPass(tail)
      case Loop(Seq(inner: Loop)) :: tail => optimizerPass(inner :: tail)

      // Merge
      case Mutate(a) :: Mutate(b) :: tail => optimizerPass(Mutate(a + b) :: tail)
      case Jump(a) :: Jump(b) :: tail => optimizerPass(Jump(a + b) :: tail)
      //case Assign(_) :: Assign(b) :: tail => optimizerPass(Assign(b) :: tail)

      // Optimize for Assign
      //case Loop(Seq(Mutate(-1))) :: Mutate(value) :: tail => optimizerPass(Assign(value) :: tail)
      //case Loop(Seq(Mutate(-1))) :: tail => optimizerPass(Assign(0) :: tail)
      //case Assign(a) :: Mutate(b) :: tail => optimizerPass(Assign(a + b) :: tail)

      // Deep-optimize loops
      case Loop(inner) :: tail => Loop(optimizerPass(inner.toList)) :: optimizerPass(tail)

      // BoomerangMutate
      //case Jump(a) :: Mutate(b) :: Jump(c) :: tail if a == -c => optimizerPass(BoomerangMutate(a, b) :: tail)
      //case BoomerangMutate(0, b) :: tail => optimizerPass(Mutate(b) :: tail)
      //case BoomerangMutate(_, 0) :: tail => optimizerPass(tail)

      // Move on
      case head :: tail => head :: optimizerPass(tail)
      case other => other
    }
  }

  def size(program: Seq[IRNode]): Int = {
    program.map {
      case Loop(body) => 1 + size(body)
      case _ => 1
    }.sum
  }

  @tailrec
  def optimize(program: Seq[IRNode]): Seq[IRNode] = {
    println(size(program))
    val opt = optimizerPass(program.toList)
    if (size(opt) < size(program)) optimize(opt)
    else opt.toArray.toSeq
  }
}
