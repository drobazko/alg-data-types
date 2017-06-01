package Adt.tree

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def generate = Branch(Branch(Leaf(10), Leaf(20)), Branch(Leaf(5), Leaf(7)))

  def fold[A,B](t: Tree[A], z: B)(f: (B, B) => B): B = t match {
    case Leaf(_) => z
    case Branch(l, r) => f(fold(l, z)(f), fold(r, z)(f))
  }

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def size2[A](t: Tree[A]): Int =
    fold(t, 1)((l, r) => 1 + l + r)

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

}