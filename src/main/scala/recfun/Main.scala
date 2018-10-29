package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      val first = 1
      val last = first

      def factorial(x: Int): Int = {
        @tailrec
        def loop(acc: Int, x: Int): Int = {
          if (x <= 0) acc
          else loop(acc * x, x - 1)
        }
        loop(1, x)
      }

      def polynom(c: Int, r: Int): Int = {
        if(c == 0) first
        else factorial(c) / (factorial(r) * factorial(c - r))
      }

      if (c == 0) first
      else if (c == r) last
      else polynom(r -1, c) + polynom(r - 1, c -1)
    }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    assert(chars.nonEmpty, "Charset is empty")

    var bufCounter: Int = 0
    var balance: Int = 0
    val open: Char = '('
    val close: Char = ')'

    def loop(list: List[Char]): Boolean = {
      if (list.isEmpty) bufCounter == 0
      else {
        val c: Char = list.head
        if (c == open) {
          bufCounter += 1
          balance += 1
        }
        if(c == close) {
          bufCounter -= 1
          if (balance > 0) balance -= 1
        }
        loop(list.tail)
      }
    }

    loop(chars)
    bufCounter == 0 && balance == 0
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
