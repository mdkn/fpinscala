package fpinscala.gettingstarted

object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  //recursive method go needs result type.
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if(n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial value of %d is %d"
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def finsFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if(n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    loop(0)
  }

  def findFirst1[A](ss: Array[A], p:A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if(n >= ss.length) -1
      else if(p(ss(n))) n
      else loop(n + 1)
    loop(0)
  }

  def fib(n: Int): Int = {
    def go(a: Int, b: Int, n: Int): Int =
      if(n <= 0) a
      else go(b, a + b, n - 1)
    go(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if(n >= as.length) true
      else if(!ordered(as(n-1), as(n))) false
      else go(n + 1)
    }
    go(1)
  }
  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatResult("absolute value", -42, abs))
    println(fib(6))
    println(isSorted(Array(1,2,3), (a: Int,b: Int) => a < b))
    println(isSorted(Array(4,2,3), (a: Int,b: Int) => a < b))
  }

  def partical1[A, B, C](a: A, f:(A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}