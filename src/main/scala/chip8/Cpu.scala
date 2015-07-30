package chip8

/**
 * @author Jaspreet
 */
case class Cpu(
  pc: Address,
  registers: RegisterFile,
  i: Register,
  stack: List[Address],
  memory: Vector[Byte],
  delayTimer: Int,
  soundTimer: Int,
  screen: Vector[Vector[Boolean]]) {

  def readInstruction(): OpCode = memory(i) << 8 | memory(i + 1)
  def renderScreen(): Unit = {

  }
}