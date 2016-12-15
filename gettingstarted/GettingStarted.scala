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

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
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
}