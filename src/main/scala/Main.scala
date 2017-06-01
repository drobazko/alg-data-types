package Adt

import Adt.list._
import Adt.tree._

object Main {
  def main(args: Array[String]) = {
    val l1 = List(1, 4, 3, 4, 5, 4,1000)
    val l2 = List(6, 7, 8)
    val l3 = List(1, 2, 3, 6, 7, 7, 8, 9, 1000)
    val l5 = List(1, 1, 1)
    val l6 = List(1, 2, 3, 4)
    val l7 = List(1, 2, 3)
    val l4 = Cons(3, Cons(2, Nil))
    val t = Branch(Branch(Branch(Leaf(1000), Leaf(2)), Leaf(20)), Branch(Branch(Branch(Leaf(1000), Leaf(2)), Leaf(20)), Leaf(27)))
    val t2 = 1 :: 2 :: scala.collection.immutable.Nil

    println(l1.getClass)
    println(l4.getClass)
    println(l4.tail)

    println(List.drop(l1, 4))
    println(List.setHead(l1, 100))
    println(List.dropWhile(l3, (n: Int) => n % 2 == 0))
    println(List.length(l1))
    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println(List.foldLeft(l2, 1)(_ * _)) // *
    println(List.foldLeft(l2, 0)(_ + _)) // +
    println(List.foldLeft(l3, 0)((acc, _) => acc + 1)) // length
    println(List.getLast(l3, 3))
    println(List.reverse(l2, Nil:List[Int])((l, v) => Cons(v, l)))
    println(List.reverse2(l2))
    println(List.append(l1, l2))
    println(List.append2(l1, l2))
    println(List.append3(l1, l2))
    println(List.addOne(l1))
    println(List.map(l1)(_.toFloat))
    println(List.filter(l1)((i:Int) => i % 2 == 0))
    println(List.flatMap(List(1,2,3))(i => List(i,i)))
    println(List.filter1(l1)((i:Int) => i == 4))
    println(List.addPairwise(l5, l2))
    println(List.zipWith(l5, l2)(_ - _))
    println(List.hasSubsequence(l6, l7))
    println(Tree.size(t))
    println(Tree.size2(t))
    println(Tree.maximum(t))
    println(Tree.depth(t))
    println(Tree.map(t)(_ + 1000))
    println("This is the final statement")
  }
}