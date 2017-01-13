package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tr: Tree[A]): Int =
    tr match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(tr: Tree[Int]): Int =
    tr match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](tr: Tree[A]): Int =
    tr match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A, B](tr: Tree[A])(f: A => B): Tree[B] =
    tr match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
}

