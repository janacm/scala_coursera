package recfun

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]) {
    for (row <- 0 to 10) {
      for (col <- 0 to row){
        print(pascal(col, row) + " ")
      }
      println()
    }
  }

  /**
    * Exercise 2
    *  For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
    */
  def pascal(c: Int, r: Int): Int = {
    val pascalsTriangle: Array[Array[Int]] = buildTriangle(r)
    pascalsTriangle(r)(c)
  }

  /**
    *
    * @param r Row to build triangle till
    * @return Array of Array of Ints that represents Pascals Triangle
    *
    * Solution:
    * - Only one row is base case
    * - Any subsequent row is the previous triangle plus the new row
    * - The new row is the pair sums of the last row of the previous triangle
    *
    */
  def buildTriangle(r: Int): Array[Array[Int]] = {
    var triangle: Array[Array[Int]] = null
    if (r==0){
      triangle = Array(Array(1))
    } else if (r==1) {
      triangle = Array(Array(1), Array(1,1))
    } else {
      triangle = buildTriangle(r-1) :+ calculateNextRowGivenTriangle(buildTriangle(r-1))
    }

    triangle
  }

  /**
    * Returns the next row, given any triangle > 2 rows
    */
  def calculateNextRowGivenTriangle(arr: Array[Array[Int]]): Array[Int] = {
    val result: Array[Int] = calculateNextRow(arr(arr.length-1))
    result
  }

  /**
    * Calculates the next row given any row > 2.
    *
    */
  def calculateNextRow(arr: Array[Int]): Array[Int] = {
    if (arr.length < 2)
      throw new Exception("Array too small")
    else {
      var pairSums: Array[Int] = Array[Int](1)

      for (i <- 0 to arr.length - 2) {
        pairSums = pairSums:+ (arr(i + 1) + arr(i))
      }
      pairSums = pairSums:+ 1
      pairSums
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    var stack: mutable.Stack[Char] = new mutable.Stack[Char]()
    var closeBrackets: mutable.Stack[Char] = new mutable.Stack[Char]()
    for (elem <- chars) {
      if (elem == '(' || elem == ')')
        stack.push(elem)
    }
    var balance = true
    while (stack.nonEmpty){
      var curr = stack.pop
      if (curr == ')'){
        closeBrackets.push(curr)
      }else {
        if (closeBrackets.isEmpty){
          balance = false
        } else {
          closeBrackets.pop()
        }
      }
    }
    if (closeBrackets.nonEmpty) balance = false
    balance
  }

  def balance_recursive(chars: List[Char]): Boolean = {
    if (chars.size < 2) {
      chars.toString().contains("()")
    } else {
      balance_recursive(chars.tail)
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
