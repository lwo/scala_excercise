package week4.numbers

abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat

  def *(that: Nat): Nat

  def /(that: Nat): Nat

  def ==(that: Nat): Boolean

  def <(that: Nat): Boolean

  def <=(that: Nat): Boolean

  def >=(that: Nat): Boolean

  def >(that: Nat): Boolean
}

object Zero extends Nat {
  def isZero = true

  def predecessor = throw new Error("0.predecessor")

  def +(that: Nat): Nat = that

  def -(that: Nat): Zero.type = if (that.isZero) this else throw new Error("negative number")

  def *(that: Nat): Nat = Zero

  def /(that: Nat): Nat = Zero

  def ==(that: Nat): Boolean = that.isZero

  def <(that: Nat): Boolean = !that.isZero

  def <=(that: Nat): Boolean = true

  def >=(that: Nat): Boolean = that.isZero

  def >(that: Nat): Boolean = throw new Error("negative number")

  override def toString = "0 => Zero"
}

class Succ(n: Nat) extends Nat {
  def isZero = false

  def predecessor: Nat = n

  def +(that: Nat): Nat = (predecessor + that).successor

  def -(that: Nat): Nat = if (that.isZero) this else predecessor - that.predecessor

  def *(that: Nat): Nat = {
    def product(acc: Nat, x: Nat): Nat = {
      if (x.isZero)
        acc
      else
        product(acc + this, x.predecessor)
    }

    if (this.isZero || that.isZero) Zero else product(this, that.predecessor)
  }

  def /(that: Nat): Nat = {
    that
  }

  def ==(that: Nat): Boolean = if (that.isZero) this.isZero else predecessor == that.predecessor

  def <(that: Nat): Boolean = !that.isZero && predecessor < that.predecessor

  def <=(that: Nat): Boolean = this == that || this < that

  def >=(that: Nat): Boolean = this == that || this > that

  def >(that: Nat): Boolean = predecessor > that.predecessor

  override def toString: String = {
    def findNumber(nat: Nat): Int =
      if (nat.isZero) 0
      else 1 + findNumber(nat.predecessor)

    val number = findNumber(this)
    String.valueOf(number) + " => " +
      ((1 to number) fold "Zero") ((s, _) => "Succ(" + s + ")")
  }
}