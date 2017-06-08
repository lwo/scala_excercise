import scala.util.Random

abstract class IntSet {
  def contains(x: Int): Boolean
  def include(x: Int): IntSet
  def union(other: IntSet): IntSet
  def el: Int
}

object Empty extends IntSet {
  override def toString = ""
  def contains(x: Int): Boolean = false
  def include(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other

  override def el: Int = 0
}

class NonEmpty(element: Int, left: IntSet, right: IntSet) extends IntSet {
  override def toString: String = "{" + left + element + right + "}"
  def contains(x: Int): Boolean =
    if (x < element) left contains x
    else if (x > element) right contains x
    else true
  def include(x: Int): IntSet =
    if (x < element) {
      new NonEmpty(element, left include x, right)
    }
    else if (x > element) {
      new NonEmpty(element, left, right include x)
    }
    else this
  def union(other: IntSet): IntSet = ((left union right) union other) include element
  override def el: Int = element
}

def fill(n: Int): IntSet = {
  def _fill(n: Int, intset: IntSet): IntSet = {
    //println(intset)
    if (n == 0) intset
    else {
      _fill(n - 1, intset.include(Random.nextInt(1000)))
    }
}
  _fill(n, Empty)
}

val inset: IntSet = fill(10)
