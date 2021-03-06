package fpinscala.datastructure

/**
  * Created by madokan on 12/15/16.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }
  //Expression of type Cons[Any] doesn't conformed to expected type List[A]
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, xs) => xs
    case _ => sys.error("empty list.")
  }

  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Cons(_, xs) => Cons(a, xs)
    case _ => sys.error("empty list.")
  }

  /*
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Cons(_, _) if n == 0 => as
    case Cons(_, xs) if n >= 1 => drop(xs, n - 1)
    case _ => sys.error("as is empty list or n is negative.")
  }
  */

  def drop[A](as: List[A], n: Int): List[A] =
    if (n == 0) as
    else as match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(x, xs) if f(x) => as
    case Nil => Nil
    case Cons(_, xs) => dropWhile(xs, f)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println(List.sum(List(1, 2, 3)))
    println(List.x)
    println(List.tail(List(1,2,3,4)))
    //println(List.tail(Nil)) => Exception in thread "main" java.lang.RuntimeException: empty
    println(List.tail(List(1)))
    println(List.setHead(List(1,2,3,4), 5))
    println(List.drop(List(1,2,3,4),2))
  }
}
