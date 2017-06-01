package Adt.list

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
  case Nil => 0 // The sum of the empty list is 0.
  case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
}

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, _) if n == 0 => l
      case Cons(_, t) => drop(t, n - 1)
    }

  def drop2[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop2(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => Cons(h, dropWhile(t, f))
      case Cons(_, t) => dropWhile(t, f)
    }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile2(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = sys.error("todo")

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightForFilter[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def foldLeftHelper(l2: List[A], acc: B): B =
      l2 match {
        case Nil => acc
        case Cons(h, t) => foldLeftHelper(t, f(acc, h))
      }

    foldLeftHelper(l, z)
  }

  /*
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
  */

  @annotation.tailrec
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft2(t, f(z,h))(f)
  }

  def append[A](l1: List[A], l2: List[A]): List[A] =
    l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft2(l1, l2)((z,h) => Cons(h, z))

  def append3[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_,_))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  def getLast[A](l: List[A], prev: A): A =
    l match {
      case Nil => prev
      case Cons(h, t) => getLast(t, h)
    }

  def reverse[A,B](l: List[A], z: List[B])(f: (List[B], A) => List[B]): List[B] =
    l match {
      case Nil => z
      case Cons(h, t) => reverse(t, f(z, h))(f)
    }

  // implementation from the book
  def reverse2[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filter1[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)((i) => if (f(i)) List(i) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def areEqual[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1,t1), Cons(h2,t2)) => if (h1 == h2) areEqual(t1, t2) else false
    }

    sup match {
      case Nil => false
      case Cons(_, t) => if (!areEqual(sup, sub)) hasSubsequence(t, sub) else true
    }
  }
}
