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

  def fold[A, B](tr: Tree[A])(l: A => B)(b: (B, B) => B): B =
    tr match {
      case Leaf(v) => l(v)
      case Branch(left, right) =>
        b(fold(left)(l)(b), fold(right)(l)(b))
    }

  def sizeViaFold[A](tr: Tree[A]): Int =
    fold(tr)(_ => 1)(1 + _ + _)

  def maximumViaFold(tr: Tree[Int]): Int =
    fold(tr)(x => x)(_ max _)

  def depthViaFold[A](tr: Tree[A]): Int =
    fold(tr)(_ => 0)((d1, d2) => 1 + (d1 max d2))

  def mapViaFold[A, B](tr: Tree[A])(f: A => B): Tree[B] =
    fold(tr)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}

