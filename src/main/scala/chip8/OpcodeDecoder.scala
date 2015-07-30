package chip8

/**
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
      case 0x9 => op9XY0
      case 0xA => opANNN
      case 0xB => opBNNN
      case 0xC => opCXNN
      case 0xD => opDXYN
      case 0xE => opcode.NN match {
        case 0x9E => opEX9E
        case 0xA1 => opEXA1
      }
      case 0xF => opcode.NN match {
        case 0x07 => opFX07
        case 0x0A => opFX0A
        case 0x15 => opFX15
        case 0x18 => opFX18
        case 0x1E => opFX1E
        case 0x29 => opFX29
        case 0x33 => opFX33
        case 0x55 => opFX55
        case 0x65 => opFX65
      }
    }

    instruction(opcode)
  }

  /* 00E0	Clears the screen. */
  val op00E0: OpInstruction = _ =>
  /* 00EE	Returns from a subroutine. */
  val op00EE: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.stack.head, stack = cpu.stack.tail)
  /* 1NNN	Jumps to address NNN. */
  val op1NNN: OpInstruction = opcode => cpu => cpu.copy(pc = opcode.NNN)
  /* 2NNN	Calls subroutine at NNN. */
  val op2NNN: OpInstruction = opcode => cpu => cpu.copy(pc = opcode.NNN, stack = cpu.pc :: cpu.stack)
  /* 3XNN	Skips the next instruction if VX equals NN. */
  val op3XNN: OpInstruction = opcode => cpu => if (cpu.registers(opcode.X) == opcode.NN) cpu.copy(pc = cpu.pc + 4) else cpu.copy(pc = cpu.pc + 2)
  /* 4XNN	Skips the next instruction if VX doesn't equal NN. */
  val op4XNN: OpInstruction = opcode => cpu => if (cpu.registers(opcode.X) != opcode.NN) cpu.copy(pc = cpu.pc + 4) else cpu.copy(pc = cpu.pc + 2)
  /* 5XY0	Skips the next instruction if VX equals VY. */
  val op5XY0: OpInstruction = opcode => cpu => if (cpu.registers(opcode.X) == cpu.registers(opcode.Y)) cpu.copy(pc = cpu.pc + 4) else cpu.copy(pc = cpu.pc + 2)
  /* 6XNN	Sets VX to NN. */
  val op6XNN: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, opcode.NN))
  /* 7XNN	Adds NN to VX. */
  val op7XNN: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, cpu.registers((opcode.X) + opcode.NN) & 0xFF))
  val op8XY0: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, cpu.registers(opcode.Y)))
  val op8XY1: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, cpu.registers(opcode.X) | cpu.registers(opcode.Y)))
  val op8XY2: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, cpu.registers(opcode.X) & cpu.registers(opcode.Y)))
  val op8XY3: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, cpu.registers(opcode.X) ^ cpu.registers(opcode.Y)))
  val op8XY4: OpInstruction = opcode => cpu => {
    val vF: Register = ((cpu.registers(opcode.X) + cpu.registers(opcode.Y)) & 0x100) >> 8
    val vX: Register = (cpu.registers(opcode.X) + cpu.registers(opcode.Y)) & 0xFF
    cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  val op8XY5: OpInstruction = opcode => cpu => {
    val vF: Register = ((cpu.registers(opcode.X) - cpu.registers(opcode.Y)) & 0x100) >> 8
    val vX: Register = (cpu.registers(opcode.X) - cpu.registers(opcode.Y)) & 0xFF
    cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  //TODO: Check type of bit shift
  val op8XY6: OpInstruction = opcode => cpu => {
    val vF: Register = cpu.registers(opcode.X) & 0x1
    val vX: Register = cpu.registers(opcode.X) >> 1
    cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  val op8XY7: OpInstruction = opcode => cpu => {
    val vF: Register = ((cpu.registers(opcode.Y) - cpu.registers(opcode.X)) & 0x100) >> 8
    val vX: Register = (cpu.registers(opcode.Y) - cpu.registers(opcode.X)) & 0xFF
    cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  val op8XYE: OpInstruction = opcode => cpu => {
    val vF: Register = (cpu.registers(opcode.X) & 0x80) >> 7
    val vX: Register = cpu.registers(opcode.X) << 1
    cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, vX).updated(0xF, vF))
  }
  val op9XY0: OpInstruction = opcode => cpu => if (cpu.registers(opcode.X) != cpu.registers(opcode.Y)) cpu.copy(pc = cpu.pc + 4) else cpu.copy(pc = cpu.pc + 2)
  val opANNN: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 2, i = opcode.NNN)
  val opBNNN: OpInstruction = opcode => cpu => cpu.copy(pc = opcode.NNN + cpu.registers(0))
  val opCXNN: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 2, registers = cpu.registers.updated(opcode.X, (Math.random() * 256).toInt & opcode.NN))
  val opDXYN: OpInstruction = ???
  val opEX9E: OpInstruction = ???
  val opEXA1: OpInstruction = ???
  /*
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
  val opFX07: OpInstruction = ???
  val opFX0A: OpInstruction = ???
  val opFX15: OpInstruction = ???
  val opFX18: OpInstruction = ???
  val opFX1E: OpInstruction = opcode => cpu => cpu.copy(pc = cpu.pc + 2, i = cpu.i + cpu.registers(opcode.X))
  val opFX29: OpInstruction = ???
  /* FX33	Stores the Binary-coded decimal representation of VX, with the most significant of three digits at the address in I, the middle digit at I plus 1, and the least significant digit at I plus 2. (In other words, take the decimal representation of VX, place the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2.) */
  val opFX33: OpInstruction = opcode => cpu => {
    val value = cpu.registers(opcode.X)
    val ones: Byte = value % 10 toByte
    val tens: Byte = (value / 10) % 10 toByte
    val hundreds: Byte = (value / 100) % 10 toByte

    val mem = cpu.memory.updated(cpu.i, hundreds).updated(cpu.i + 1, tens).updated(cpu.i + 2, ones)
    cpu.copy(pc = cpu.pc + 2, memory = mem)
  }
  val opFX55: OpInstruction = opcode => cpu => cpu.copy(
    pc = cpu.pc + 2,
    memory = (0 to opcode.X).foldLeft(cpu.memory)((memory, reg) => memory.updated(reg, cpu.registers(reg).toByte))
  )
  val opFX65: OpInstruction = opcode => cpu => cpu.copy(
    pc = cpu.pc + 2,
    registers = (0 to opcode.X).foldLeft(cpu.registers)((registers, reg) => registers.updated(reg, cpu.memory(cpu.i + reg)))
  )
}