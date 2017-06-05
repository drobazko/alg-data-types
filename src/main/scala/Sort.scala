package Adt.sorting

object Sorting {
  // Merge of two sorted arrays via recursion
  def merge(a: Array[Int], b: Array[Int]): Array[Int] = {
    if (a.isEmpty) return b
    if (b.isEmpty) return a
    if (a(0) < b(0))
      a(0) +: merge2(a.slice(1, a.length), b)
    else
      b(0) +: merge2(a, b.slice(1, b.length))
  }

  // Merge of two sorted arrays via recursion
  def merge2(a: Array[Int], b: Array[Int]): Array[Int] = {
    var res = Array[Int]()
    var (aPointer, bPointer) = (0, 0)

    while (res.length < a.length + b.length ) {
      if (aPointer >= a.length) res = res ++ b.slice(bPointer, b.length)
      if (bPointer >= b.length) res = res ++ a.slice(aPointer, a.length)

      if (aPointer < a.length && bPointer < b.length && a(aPointer) < b(bPointer)) {
        res = res :+ a(aPointer)
        aPointer = aPointer + 1
      } else if (aPointer < a.length && bPointer < b.length) {
        res = res :+ b(bPointer)
        bPointer = bPointer + 1
      }
    }
    res
  }
}