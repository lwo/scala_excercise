val l1 = List()
var l2 = List(1)
val l3 = List(2, 3, 2, 1)

def printout[T](list: List[T]): Unit = {
  if (list.isEmpty)
    println("Nil)")
  else {
    println("Const(" + list.head + ", ")
    printout(list.tail)
  }
}

printout(l1)
printout(l2)
printout(l3)
