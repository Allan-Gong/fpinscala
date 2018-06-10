package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = {
    case None => default
    case Some(b:B) => b
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    case None => None
    case Some(a:A) => f(a)
  }

  def flatMapWithMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    case None => ob
    case Some(b:B) => Some(b)
  }

  def filter(f: A => Boolean): Option[A] = {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val mean = mean(xs)

    val variance = xs.foldLeft(0.0)((acc, current) => {
      acc + math.pow(current - mean, 2)
    }) / xs.size

    Some(variance)
  }

  def varianceWithFlatMap(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    )
  }

  def map2[A,B,C](optionA: Option[A], optionB: Option[B])(f: (A, B) => C): Option[C] = (optionA, optionB ) match {
    case (None, _) || (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  def map2WithFlatMap[A,B,C](optionA: Option[A], optionB: Option[B])(f: (A, B) => C): Option[C] =
    optionA flatMap (a => optionB map (b => f(a, b)))

  def map2WithForComprehension[A,B,C](optionA: Option[A], optionB: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- optionA
      b <- optionB
    } yield {
      f(a, b)
    }

  def sequence[A](list: List[Option[A]]): Option[List[A]] = list match {
    case Nil => Some(Nil)
    case None :: _ => None
    case Some(a) :: Nil => Some(a :: Nil)
    case Some(a) :: tail => sequence(tail).map(t => a :: t)
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def sequenceOfficial[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case head :: tail => head flatMap (headValue => sequence(tail) map (headValue :: _))
    }

  // not efficient
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val list: List[Option[B]] = a map f
    sequence(list)
  }

  def traverseViaMap2[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] =
    list match {
      case Nil => Some(Nil)
      case head :: tail => map2(f(head), traverseViaMap2(tail)(f))((headB, tailB) => headB :: tailB)
    }
}
