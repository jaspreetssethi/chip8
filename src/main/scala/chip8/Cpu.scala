package chip8

/**
 * @author Jaspreet
 */
case class Cpu(
    val pc: Address,
    val registers: RegisterFile,
    val i: Register,
    val stack: List[Address],
    val memory: Vector[Byte]) {

}