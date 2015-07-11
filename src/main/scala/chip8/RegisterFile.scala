package chip8

/**
 * @author Jaspreet
 */
case class RegisterFile(
  val v0: Register,
  val v1: Register,
  val v2: Register,
  val v3: Register,
  val v4: Register,
  val v5: Register,
  val v6: Register,
  val v7: Register,
  val v8: Register,
  val v9: Register,
  val vA: Register,
  val vB: Register,
  val vC: Register,
  val vD: Register,
  val vE: Register,
  val vF: Register) {}