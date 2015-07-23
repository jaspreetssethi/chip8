package chip8

/**
 * 00E0	Clears the screen.
 * 00EE	Returns from a subroutine.
 * 1NNN	Jumps to address NNN.
 * 2NNN	Calls subroutine at NNN.
 * 3XNN	Skips the next instruction if VX equals NN.
 * 4XNN	Skips the next instruction if VX doesn't equal NN.
 * 5XY0	Skips the next instruction if VX equals VY.
 * 6XNN	Sets VX to NN.
 * 7XNN	Adds NN to VX.
 * 8XY0	Sets VX to the value of VY.
 * 8XY1	Sets VX to VX or VY.
 * 8XY2	Sets VX to VX and VY.
 * 8XY3	Sets VX to VX xor VY.
 * 8XY4	Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't.
 * 8XY5	VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
 * 8XY6	Shifts VX right by one. VF is set to the value of the least significant bit of VX before the shift.[2]
 * 8XY7	Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
 * 8XYE	Shifts VX left by one. VF is set to the value of the most significant bit of VX before the shift.[2]
 * 9XY0	Skips the next instruction if VX doesn't equal VY.
 * ANNN	Sets I to the address NNN.
 * BNNN	Jumps to the address NNN plus V0.
 * CXNN	Sets VX to a random number, masked by NN.
 * DXYN	Sprites stored in memory at location in index register (I), maximum 8bits wide. Wraps around the screen. If when drawn, clears a pixel, register VF is set to 1 otherwise it is zero. All drawing is XOR drawing (i.e. it toggles the screen pixels)
 * EX9E	Skips the next instruction if the key stored in VX is pressed.
 * EXA1	Skips the next instruction if the key stored in VX isn't pressed.
 * FX07	Sets VX to the value of the delay timer.
 * FX0A	A key press is awaited, and then stored in VX.
 * FX15	Sets the delay timer to VX.
 * FX18	Sets the sound timer to VX.
 * FX1E	Adds VX to I.[3]
 * FX29	Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font.
 * FX33	Stores the Binary-coded decimal representation of VX, with the most significant of three digits at the address in I, the middle digit at I plus 1, and the least significant digit at I plus 2. (In other words, take the decimal representation of VX, place the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2.)
 * FX55	Stores V0 to VX in memory starting at address I.[4]
 * FX65	Fills V0 to VX with values from memory starting at address I.
 */
object OpcodeDecoder {
  def decode(opcode: OpCode): CpuInstruction = {
    val instruction = opcode.instruction match {
      case 0x0 => opcode.NNN match {
        case 0x0E0 => op00E0
        case 0x0EE => op00EE
      }
      case 0x1 => op1NNN
      case 0x2 => op2NNN
      case 0x3 => op3XNN
      case 0x4 => op4XNN
      case 0x5 => op5XY0
      case 0x6 => op6XNN
      case 0x7 => op7XNN
      case 0x8 => opcode.N match {
        case 0x0 => op8XY0
        case 0x1 => op8XY1
        case 0x2 => op8XY2
        case 0x3 => op8XY3
        case 0x4 => op8XY4
        case 0x5 => op8XY5
        case 0x6 => op8XY6
        case 0x7 => op8XY7
        case 0xE => op8XYE
      }
      case 0x9 => ???
      case 0xA => ???
      case 0xB => ???
      case 0xC => ???
      case 0xD => ???
      case 0xE => ???
      case 0xF => ???
    }

    instruction(opcode)
  }

  val op00E0: OpInstruction = ???
  val op00EE: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.stack.head, stack = cpu.stack.tail)
  val op1NNN: OpInstruction = opcode => cpu => cpu.copy(pc = opcode.NNN)
  val op2NNN: OpInstruction = opcode => cpu => cpu.copy(pc = opcode.NNN, stack = cpu.pc :: cpu.stack)
  val op3XNN: OpInstruction = opcode => cpu => if (cpu.registers(opcode.X) == opcode.NN) cpu.copy(pc = cpu.pc + 2) else cpu.copy(pc = cpu.pc + 1)
  val op4XNN: OpInstruction = opcode => cpu => if (cpu.registers(opcode.X) != opcode.NN) cpu.copy(pc = cpu.pc + 2) else cpu.copy(pc = cpu.pc + 1)
  val op5XY0: OpInstruction = opcode => cpu => if (cpu.registers(opcode.X) == cpu.registers(opcode.Y)) cpu.copy(pc = cpu.pc + 2) else cpu.copy(pc = cpu.pc + 1)
  val op6XNN: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, opcode.NN))
  val op7XNN: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, cpu.registers((opcode.X) + opcode.NN) & 0xFF))
  val op8XY0: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, cpu.registers(opcode.Y)))
  val op8XY1: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, cpu.registers(opcode.X) | cpu.registers(opcode.Y)))
  val op8XY2: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, cpu.registers(opcode.X) & cpu.registers(opcode.Y)))
  val op8XY3: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, cpu.registers(opcode.X) ^ cpu.registers(opcode.Y)))
  val op8XY4: OpInstruction = opcode => cpu => {
    val vF:Register = ((cpu.registers(opcode.X) + cpu.registers(opcode.Y)) & 0x100) >> 8
    val vX:Register = (cpu.registers(opcode.X) + cpu.registers(opcode.Y)) & 0xFF
    cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  val op8XY5: OpInstruction = opcode => cpu => {
    val vF:Register = ((cpu.registers(opcode.X) - cpu.registers(opcode.Y)) & 0x100) >> 8
    val vX:Register = (cpu.registers(opcode.X) - cpu.registers(opcode.Y)) & 0xFF
    cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  //TODO: Check type of bit shift
  val op8XY6: OpInstruction = opcode => cpu => {
    val vF:Register = cpu.registers(opcode.X) & 0x1
    val vX:Register = cpu.registers(opcode.X) >> 1
    cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  val op8XY7: OpInstruction = opcode => cpu => {
    val vF:Register = ((cpu.registers(opcode.Y) - cpu.registers(opcode.X)) & 0x100) >> 8
    val vX:Register = (cpu.registers(opcode.Y) - cpu.registers(opcode.X)) & 0xFF
    cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  val op8XYE: OpInstruction = opcode => cpu => {
    val vF:Register = (cpu.registers(opcode.X) & 0x80) >> 7
    val vX:Register = cpu.registers(opcode.X) << 1
    cpu.copy(pc = cpu.pc + 1, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  val op9XY0: OpInstruction = opcode => cpu => if (cpu.registers(opcode.X) != cpu.registers(opcode.Y)) cpu.copy(pc = cpu.pc + 2) else cpu.copy(pc = cpu.pc + 1)
}