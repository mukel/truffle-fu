package brainfck

import java.io.{InputStream, OutputStream}

import brainfck.Brainfck._
import org.cojen.classfile.Opcode

/**
  * Dynamically compiles a Brainfck program into bytecode.
  */
class BrainfckBytecode extends BrainfckImpl {

  import org.cojen.classfile.{CodeBuilder, Modifiers, RuntimeClassFile, TypeDesc}

  /**
    * Compiles the Brainfck program to Bytecode.
    */
  def compile(program: Seq[IRNode]): RuntimeClassFile = {
    // Create a ClassFile with the super class of Object.
    val cf = new RuntimeClassFile("PureBytecodeBrainfck")

    // Create some types which will be needed later.
    val inputStream = TypeDesc.forClass("java.io.InputStream")
    val outputStream = TypeDesc.forClass("java.io.OutputStream")

    // Add the run(in, out) method, and construct a CodeBuilder for defining the
    // bytecode.
    val params = Array[TypeDesc](inputStream, outputStream)
    val runMethod = cf.addMethod(Modifiers.PUBLIC_STATIC, "run", null, params)

    val b = new CodeBuilder(runMethod)

    // ptr = 0
    val ptr = b.createLocalVariable("ptr", TypeDesc.INT)
    b.loadConstant(0)
    b.storeLocal(ptr)

    // tape = Array.ofDim[Int](1 << 16)(0)
    val tape = b.createLocalVariable("tape", TypeDesc.INT.toArrayType)
    b.loadConstant(1 << 16)
    b.newObject(TypeDesc.INT.toArrayType, 1)
    b.storeLocal(tape)

    val inParam = 0 // in
    val outParam = 1 // out

    def compile(op: IRNode): Unit = {
      op match {
        case Jump(delta) =>
          b.integerIncrement(ptr, delta)

        case Assign(value) =>
          // tape(ptr) = value
          b.loadLocal(tape)
          b.loadLocal(ptr)
          b.loadConstant(value)
          b.storeToArray(TypeDesc.INT)

        case Mutate(delta) =>
          // tape(ptr) += delta
          b.loadLocal(tape)
          b.loadLocal(ptr)
          b.dup2()
          b.loadFromArray(TypeDesc.INT)
          b.loadConstant(delta)
          b.math(Opcode.IADD)
          b.storeToArray(TypeDesc.INT)

        case BoomerangMutate(deltaPtr, deltaValue) =>
          // tape(ptr + deltaPtr) += deltaValue
          b.loadLocal(tape)
          b.loadLocal(ptr)
          b.loadConstant(deltaPtr)
          b.math(Opcode.IADD)
          b.dup2()
          b.loadFromArray(TypeDesc.INT)
          b.loadConstant(deltaValue)
          b.math(Opcode.IADD)
          b.storeToArray(TypeDesc.INT)

        case Read =>
          // tape(ptr) = in.read()
          b.loadLocal(tape)
          b.loadLocal(ptr)
          b.loadLocal(b.getParameter(inParam))
          b.invokeVirtual(inputStream, "read", TypeDesc.INT, null)
          b.storeToArray(TypeDesc.INT)

        case Write =>
          // out.write(tape(ptr))
          b.loadLocal(b.getParameter(outParam))
          b.loadLocal(tape)
          b.loadLocal(ptr)
          b.loadFromArray(TypeDesc.INT)
          b.invokeVirtual(outputStream, "write", null, Array(TypeDesc.INT))

          // out.flush()
          b.loadLocal(b.getParameter(outParam))
          b.invokeVirtual(outputStream, "flush", null, null)

        case Loop(body) =>
          val whileLoop = b.createLabel().setLocation()
          val endLoop = b.createLabel()

          b.loadLocal(tape)
          b.loadLocal(ptr)
          b.loadFromArray(TypeDesc.INT)

          b.ifZeroComparisonBranch(endLoop, "==")
            body.foreach(compile)
          b.branch(whileLoop)

          endLoop.setLocation()
      }
    }

    program.foreach(compile)
    b.returnVoid()

    cf
  }

  def run(program: Seq[IRNode])(in: InputStream, out: OutputStream): Unit = {
    val cf = compile(program)
    val clazz = cf.defineClass()

    // Find the generated method and invoke it.
    val m = clazz.getMethod("run", classOf[InputStream], classOf[OutputStream])
    m.invoke(null, in, out)
  }
}
