import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import week4.numbers._

trait Numbers {

  private def number(n: Int): Nat = {
    def _number(n: Int, nat: Nat): Nat = {
      if (n == 0) nat else _number(n - 1, new Succ(nat))
    }

    _number(n, Zero)
  }

  val One: Nat = number(1)
  val Two: Nat = number(2)
  val Three: Nat = number(3)
  val Four: Nat = number(4)
  val Five: Nat = number(5)
  val Six: Nat = number(6)
  val Seven: Nat = number(7)
  val Eight: Nat = number(8)
  val Nine: Nat = number(9)
}

@RunWith(classOf[JUnitRunner])
class NatSuite extends FunSuite {

  test("Comparison") {
    new Numbers {
      assert(Zero == Zero)
      assert(One == One)
      assert(!(One == Two))
    }
  }

  test("Adding") {
    new Numbers {
      assert(Zero + Zero == Zero)
      assert(Zero + One == One)
      assert(One + Zero == One)
      assert(One + One == Two)
      assert(Two + Five == Seven)
    }
  }

  test("Substraction") {
    new Numbers {
      assert(Nine - Nine == Zero)
      assert(Nine - One == Eight)
      assert(Nine - Eight == One)
      assert(One - One == Zero)
      assertThrows[Error] {
        One - Two
      }
    }
  }

  test("Multiplication") {
    new Numbers {
      assert(Zero * Zero == Zero)
      assert(One * Zero == Zero)
      assert(One * One == One)
      assert(Two * Three == Six)
      assert(Three * Two == Six)
    }
  }


}