package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case _ if (n <= 0) => this
    case Empty => this
    case Cons(h, t) => cons(h(), t().take(n-1))
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (_, _) if (n <= 0) => None
      case (Empty, _) => None
      case (Cons(h, t), _) => Some(h(), (t(), n-1))
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case _ if (n <= 0) => this
    case Empty => this
    case Cons(_, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) =>
      if (p(h())) cons(h(), t().takeWhile(p))
      else t().takeWhile(p)
  }

  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
        if (p(h)) cons(h, t)
        else empty)

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) =>
        if (p(h())) Some((h(), t()))
        else None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, s) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = ???

  def forAll(p: A => Boolean): Boolean =
    !exists(h => !p(h))

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
        if (p(h)) cons(h, t)
        else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] =
      cons(a, loop(b, a+b))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((next, state)) => cons(next, unfold(state)(f))
    }

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (a, b) => Some((a, (b, a+b))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n) { case m => Some(m, m+1) }

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a) { case _ => Some(a, a) }

  def onesViaUnfold: Stream[Int] = constantViaUnfold(1)
}

