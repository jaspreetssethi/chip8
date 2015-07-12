

package object chip8 {
  type Register = Int
  type Address = Int
  type OpCode = Int
  
  type CpuInstruction = Cpu => Cpu
  type OpInstruction = OpCode => CpuInstruction
  
  val PROGRAM_START:Int = 0x200
  
  implicit class OpcodeOps(x: OpCode) {
    def instruction = x & 0xF000 >>> 12
    def NNN = x & 0x0FFF
    def NN:Byte = (x & 0x00FF).toByte
    def N:Byte = (x & 0x000F).toByte
    
    def X = x & 0x0F00 >>> 8
    def Y = x & 0x00F0 >>> 4
  }
}