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

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = (this, s2) match {
    case (Empty, Empty) => Empty
    case (Empty, Cons(head2, tail2)) => cons((None, Some(head2())), zipAll(tail2()))
    case (Cons(head1, tail1), Empty) => cons((Some(head1()), None), tail1().zipAll(empty))
    case (Cons(head1, tail1), Cons(head2, tail2)) => cons((Some(head1()), Some(head2())), tail1().zipAll(tail2()))
    case(_, _) => Empty
  }

  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      case(_, _) => None
    }

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Empty, Empty) => true
    case (Empty, Cons(_, _)) => false
    case (Cons(_, _), Empty) => true
    case (Cons(head1, tail1), Cons(head2, tail2)) if head1() == head2() => tail1().startsWith(tail2())
  }

  def startsWithOfficial[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h,h2) => h == h2
    }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  // ???? !!!!!!
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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

  // It takes an initial state, and a function for producing both the next state(S) and the next value(A) in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
}
