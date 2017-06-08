package week4.list


trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  override def toString: String = {
    if (isEmpty)
      "Nil)"
    else
      "Const(" + head + ", "
  }
}

class Const[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nill[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("No head")

  override def tail: Nothing = throw new NoSuchElementException("No tail")
}

object List {
  def apply[T](): List[T] = new Nill()

  def apply[T](x1: T): List[T] = new Const(x1, new Nill)

  def apply[T](x1: T, x2: T): List[T] = new Const(x1, new Const(x2, new Nill))

  override def toString: String = {
    "Hello"
  }
}

object main {
  def main(args: Array[String]): Unit = {

  }
}