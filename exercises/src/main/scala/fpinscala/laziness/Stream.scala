package fpinscala.laziness

import fpinscala.laziness.Stream.{cons, empty, unfold}

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(head, tail) => head() :: tail().toList
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, _) if n == 1 => cons(head(), empty)
    case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tail) if n > 1 => tail().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) if p(head()) => tail().takeWhile(p)
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(head, tail) if p(head()) => Some((head(), tail()))
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(head, _) if !p(head()) => false
    case Cons(head, tail) if p(head()) => tail().forAll(p)
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(head, _) => Some(head())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this match {
    case Empty => Empty
    case Cons(head, tail) => cons(f(head()), tail().map(f))
  }

  def mapViaFoldRight[B](f: A => B): Stream[B] =
    foldRight(empty[B])((head, tail) => cons(f(head), tail))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Empty => None
    case Cons(head, tail) => Some(f(head()), tail())
  }

  def filter(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) if !f(head()) => tail().filter(f)
    case Cons(head, tail) if f(head()) => cons(head(), tail().filter(f))
  }

  def filterViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((head, tail) => {
      if (f(head)) cons(head, tail)
      else tail
    })

  def append[B>:A](s: => Stream[B]): Stream[B] = this match {
    case Empty => s
    case Cons(head, tail) => cons(head(), tail().append(s))
  }

  def appendViaFoldRight[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((head, tail) => cons(head, tail))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((head, tail) => tail.append(f(head)))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???

  def startsWith[B](s: Stream[B]): Boolean = ???

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def tails: Stream[Stream[A]] = ???

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???
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

  val ones: Stream[Int] = cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
}
