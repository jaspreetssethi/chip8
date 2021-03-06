package chip8

/**
 * @author Jaspreet
 */
case class RegisterFile(
  v0: Register,
  v1: Register,
  v2: Register,
  v3: Register,
  v4: Register,
  v5: Register,
  v6: Register,
  v7: Register,
  v8: Register,
  v9: Register,
  vA: Register,
  vB: Register,
  vC: Register,
  vD: Register,
  vE: Register,
  vF: Register) {

  def apply(x:Int):Register = x match {
    case 0x0 => return v0
    case 0x1 => return v1
    case 0x2 => return v2
    case 0x3 => return v3
    case 0x4 => return v4
    case 0x5 => return v5
    case 0x6 => return v6
    case 0x7 => return v7
    case 0x8 => return v8
    case 0x9 => return v9
    case 0xA => return vA
    case 0xB => return vB
    case 0xC => return vC
    case 0xE => return vE
    case 0xF => return vF
  }

  def updated(x:Int, value: Register):RegisterFile = x match {
    case 0x0 => this.copy(v0 = value)
    case 0x1 => this.copy(v1 = value)
    case 0x2 => this.copy(v2 = value)
    case 0x3 => this.copy(v3 = value)
    case 0x4 => this.copy(v4 = value)
    case 0x5 => this.copy(v5 = value)
    case 0x6 => this.copy(v6 = value)
    case 0x7 => this.copy(v7 = value)
    case 0x8 => this.copy(v8 = value)
    case 0x9 => this.copy(v9 = value)
    case 0xA => this.copy(vA = value)
    case 0xB => this.copy(vB = value)
    case 0xC => this.copy(vC = value)
    case 0xD => this.copy(vD = value)
    case 0xE => this.copy(vE = value)
    case 0xF => this.copy(vF = value)
  }
}