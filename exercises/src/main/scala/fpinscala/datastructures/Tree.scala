package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t {
    case Leaf(value) => value
    case Branch(left:Tree[Int], right:Tree[Int]) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + math.max(depth(left), depth(right))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t {
    case Leaf(v) => Leaf(f(v))
    case Branch(left:Tree[A], right:Tree[A]) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B =  t {
    case Leaf(value) => f(value)
    case Branch(left:Tree[A], right:Tree[A]) => g(fold(left)(f), fold(right)(f))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((d1,d2) => 1 + (d1 max d2))

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}
