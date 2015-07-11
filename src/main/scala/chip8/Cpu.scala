package chip8

/**
 * @author Jaspreet
 */
case class Cpu(
    val pc: Register,
    val registers: RegisterFile,
    val l: Register,
    val stack: List[Address],
    val memory) {

}