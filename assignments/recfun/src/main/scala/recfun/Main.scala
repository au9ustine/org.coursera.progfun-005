package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = (c + 1 to r).product / (1 to r - c).product

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val results: Int = 0
    def helper(chars: List[Char], results: Int): Int = {
      results match {
        case x if x < 0 => x
        case _ => chars match {
          case x if x.isEmpty => results
          case x :: xs if x.compareTo('(') == 0 => helper(chars.tail, results + 1)
          case x :: xs if x.compareTo(')') == 0 => helper(chars.tail, results - 1)
          case _ => helper(chars.tail, results)
        }
      }
    }
    helper(chars, results) match {
      case x if x == 0 => true
      case _ => false
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    money match {
      case x if x == 0 => 1
      case x if x < 0 => 0
      case _ => coins match {
        case Nil => 0
        case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)
      }
    }
  }
}
