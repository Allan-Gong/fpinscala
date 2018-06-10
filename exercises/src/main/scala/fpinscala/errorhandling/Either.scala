package fpinscala.errorhandling


import scala.{Either => _, Left => _, Option => _, Right => _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = {
   case Left(e) => Left(e)
   case Right(a) => Right(f(a))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
   case Left(e) => Left(e)
   case Right(a:A) => f(a)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
   case Left(_) => b
   case _ => _
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   for {
     aa <- this
     bb <- b
   } yield {
     f(aa, bb)
   }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    es match {
      case Nil => Right(Nil)
      case head :: tail => head flatMap (headValue => sequence(tail) map (headValue :: _))
    }
  }

  def sequenceViaTraverse[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val listOfEither = es.map(f)
    sequence(listOfEither)
  }

  def traverseOfficial[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case head :: tail => (f(head) map2 traverseOfficial(tail)(f))(_ :: _)
    }

  def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
