// Exercise 2.1
def fib(n: Int): Int = {
  @annotation.tailrec
  def loop(n: Int, current: Int, last: Int): Int = {
    if (n == 0) {
      last
    } else {
      loop(n-1, current + last, current)
    }
  }
  loop(n, 1, 0)
}

// Exercise 2.2
// test using e.g.:
// isSorted(Array(1,2,3), (x: Int, y: Int) => x <= y)
// isSorted(Array(1,2,1), (x: Int, y: Int) => x <= y)
// isSorted(Array('a','b','c'), (x: Char, y: Char) => x <= y)
// isSorted(Array('a','b','a'), (x: Char, y: Char) => x <= y)
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Boolean =
    if (n + 1 > as.length - 1) {
      true
    } else {
      if (ordered(as(n), as(n+1))) loop(n + 1)
      else false
    }
  loop(0)
}

// Exercise 2.3
def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  a => b => f(a, b)
}

// Exercise 2.4
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}

// Exercise 2.5
def compose[A,B,C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}
